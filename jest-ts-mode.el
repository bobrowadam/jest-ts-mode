;;; jest-ts-mode.el --- summary -*- lexical-binding: t -*-

;; Author: Adam Bobrow
;; Maintainer: Adam Bobrow
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1")
;;                    (s)
;;                    (dash))
;; Keywords: jest,js,typescript,tree-sitter


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;;; Code:
(require 'dash)
(require 'treesit)
(require 'ert)
(require 'ansi-color)
(require 'compile)
(require 's)

(defgroup jest-ts nil
  "Jest testing integration for Typescript."
  :group 'tools
  :prefix "jest-ts/")

(defun jest-ts/test-colorize-compilation-buffer ()
  "Colorize the compilation buffer.
Applies ANSI color codes to the compilation output.
Added as a hook to `compilation-filter-hook'."
  (ansi-color-apply-on-region compilation-filter-start (point)))

(define-compilation-mode jest-ts/compilation-mode "Jest Compilation"
  "Compilation mode for Jest output."
  (add-hook 'compilation-filter-hook 'jest-ts/test-colorize-compilation-buffer nil nil))
(define-key jest-ts/compilation-mode-map (kbd "C-c C-t C-r") 'jest-ts/rerun-latest-test)
(define-key jest-ts/compilation-mode-map (kbd "C-c C-t C-j") 'jest-ts/jump-to-latest-test)
(define-key jest-ts/compilation-mode-map (kbd "C-c C-t C-b") 'jest-ts/jump-to-compilation-buffer)

(defun jest-ts/read--file (file-name)
  "Return the contents of FILE-NAME as a string.
FILE-NAME is the path to the file to read.
Returns the file contents or nil if the file doesn't exist."
  (when (file-exists-p file-name)
    (with-temp-buffer
      (insert-file-contents file-name)
      (buffer-string))))

(defvar *latest-test* nil
  "Stores the latest test that was run.
Format is (test-file-name test-name test-point DEFAULT-DIRECTORY).")

(defun jest-ts/persist-latest-test (test-file-name test-name-and-point)
  "Store the latest test information for rerunning later.
TEST-FILE-NAME is the path to the test file.
TEST-NAME-AND-POINT is a list containing the test name and point.
Stores the information in the `*latest-test*' variable for later use."
  (setq *latest-test*
        (list test-file-name
              (car test-name-and-point)
              (cadr test-name-and-point)
              default-directory)))

(defcustom jest-ts/jest-command-fn
  (lambda ()
    (format "%snode_modules/.bin/jest" (locate-dominating-file "" "node_modules")))
  "A function that return the path to the jest executable."
  :type '(function :tag "Function")
  :group 'jest-ts)

(defcustom jest-ts/environment-variables nil
  "Custom environment variables for running the jest process.
Should be an alist of (VARIABLE . VALUE) pairs."
  :type '(alist :key-type string :value-type string)
  :group 'jest-ts)

(defcustom jest-ts/inspect-port 9229
  "The port for the node js inspector."
  :type '(choice (number :tag "Port number")
                 (function :tag "Function that returns the port number"))
  :group 'jest-ts)

;;;###autoload
(defun jest-ts/run-tests (describe-only)
  "Run a specific test from the current file.
With prefix argument DESCRIBE-ONLY, only show describe blocks for selection."
  (interactive "P")
  (let* ((default-directory (jest-ts/find--jest-config-parent-directory))
         (test-info (jest-ts/choose--test-with-completion describe-only))
         (test-name (car test-info))
         (test-point (cdr test-info))
         (test-file-name (buffer-file-name))
         (process-environment (copy-sequence process-environment)))
    (dolist (env-var jest-ts/environment-variables)
      (push (format "%s=%s"
                    (car env-var)
                    (cdr env-var))
            process-environment))
    (jest-ts/persist-latest-test test-file-name (list test-name test-point))
    (jest-ts/jump-to-latest-test)
    (compile (jest-ts/test--command
              default-directory
              `(:file-name ,test-file-name :test-name ,test-name))
             'jest-ts/compilation-mode)))

;;;###autoload
(defun jest-ts/rerun-latest-test ()
  "Run the latest test when it exists."
  (interactive)
  (if-let ((*latest-test*)
           (default-directory (nth 3 *latest-test*))
           (file-name (car *latest-test*))
           (test-name (nth 1 *latest-test*))
           (process-environment (copy-sequence process-environment)))
      (progn
        ;; Add environment variables to process-environment
        (dolist (env-var jest-ts/environment-variables)
          (push (format "%s=%s" (car env-var) (cdr env-var)) process-environment))
        (compile (jest-ts/test--command default-directory
                                        (list :file-name
                                              file-name
                                              :test-name
                                              test-name))
                 'jest-ts/compilation-mode))
    (error "Could not rerun latest test.
 *latest-test*: %s
 default directory: %s"
           *latest-test*
           default-directory)))

;;;###autoload
(defun jest-ts/jump-to-latest-test ()
  "Jump to the latest test definition when it exists."
  (interactive)
  (if-let ((*latest-test*)
           (default-directory (nth 3 *latest-test*))
           (file-name (car *latest-test*))
           (test-name (nth 1 *latest-test*))
           (test-point (nth 2 *latest-test*)))
      (progn (push-mark (point) t)
             (find-file file-name)
             (goto-char test-point))
    (error "Could not jump to latest test.
 *latest-test*: %s
 default directory: %s"
           *latest-test*
           default-directory)))

(defun jest-ts/find--jest-config-parent-directory ()
  "Find the parent directory containing jest.config.ts.
Searches upward from the current directory.
Returns the directory path or signals an error if not found."
  (or (locate-dominating-file "./" "jest.config.ts")
      (error "No jest-config found.  default directory: %s"
             default-directory)))

;;;###autoload
(defun jest-ts/run-test-at-point ()
  "Run the enclosing test around point."
  (interactive)
  (if-let ((default-directory (jest-ts/find--jest-config-parent-directory))
           (test-name-and-point (jest-ts/get--current-test-name-and-point))
           (test-file-name (buffer-file-name))
           (process-environment (copy-sequence process-environment)))
      (progn
        ;; Add environment variables to process-environment
        (dolist (env-var jest-ts/environment-variables)
          (push (format "%s=%s" (car env-var) (cdr env-var)) process-environment))
        (jest-ts/persist-latest-test test-file-name test-name-and-point)
        (compile (jest-ts/test--command default-directory
                                        (list :file-name
                                              test-file-name
                                              :test-name
                                              (car test-name-and-point)
                                              :test-point
                                              (cadr test-name-and-point) ))
                 'jest-ts/compilation-mode))
    (error "Could not run test at point:
test-name: %s
test-file-name %s" test-name-and-point test-file-name)))

(defun jest-ts/test--command (jest-config-dir &optional test-file-name-and-pattern)
  "Create the command to run Jest test.
JEST-CONFIG-DIR is the directory containing jest.config.ts.
TEST-FILE-NAME-AND-PATTERN is a plist with optional
`:file-name' and `:test-name'.
Returns a shell command string that can be passed to `compile'."
  (let* ((file-name (or (plist-get test-file-name-and-pattern :file-name) ""))
         (test-name (plist-get test-file-name-and-pattern :test-name)))
    (->>
     (format "node --inspect=%s %s --config %sjest.config.ts %s %s"
             (if (functionp jest-ts/inspect-port)
                 (funcall jest-ts/inspect-port)
                 jest-ts/inspect-port)
             (funcall jest-ts/jest-command-fn)
             jest-config-dir
             file-name
             (if test-name
                 (format "-t \"%s\""
                         test-name)
               ""))
     s-trim-right
     (s-replace-regexp "\\[\\|\\]" "\\\\\\&"))))

(defun jest-ts/is--jest-test-call (node)
  "Check if the given NODE is a Jest test function (describe | it | test).
Returns non-nil if NODE is a call to describe, it, or test function."
  (and (string= (treesit-node-type node) "call_expression")
       (let* ((function-node (treesit-node-child-by-field-name node "function"))
              (function-name (treesit-node-text function-node t)))
         (member function-name '("describe" "it" "test")))))

(defun jest-ts/get--current-test-name-and-point ()
  "Extract the current test name and position using Treesit.
Uses the treesit parser to find the enclosing test or describe block.
Returns a list of (test-name position) or nil if not in a test."
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (when (treesit-ready-p 'typescript)
      (-if-let* ((node (treesit-node-at (point)))
                 (test-node (treesit-parent-until node #'jest-ts/is--jest-test-call))
                 (test-name-node (treesit-node-child-by-field-name test-node "arguments"))
                 (first-arg-node (treesit-node-child test-name-node 1))
                 (node-type (treesit-node-type first-arg-node))
                 (node-start-point (treesit-node-start test-node)))
          (list (string-node-string-fragment first-arg-node)
                node-start-point)))))

(defun string-node-string-fragment (node)
  "Extract string content from a string NODE.
Handles both simple string nodes and binary expressions.
Returns the extracted string content."
  (cond
   ((string= (treesit-node-type node) "string")
    (treesit-node-text (nth 1 (treesit-node-children node)) t))
   ((string= (treesit-node-type node) "binary_expression") (binary--exp-to-string node))
   (t (error "Bad string node type"))))

(defun binary--exp-to-string (node)
  "Convert a binary expression NODE to a string.
Extracts and concatenates string fragments from the binary expression.
Returns the combined string."
  (mapconcat (lambda (child)
               (substring-no-properties
                (treesit-node-text
                 (get-first-node-by-type child "string_fragment"))))
             (treesit-node-children node "arguments")))

(defun template--node-to-string (node)
  "Convert a template string NODE to a string.
Extracts and concatenates all parts of the template string.
Returns the combined string."
  (mapconcat #'treesit-node-text
             (treesit-node-children node "arguments")))

(defun jest-ts/choose--test-with-completion (&optional describe-only)
  "Choose a test using completion.
If DESCRIBE-ONLY is non-nil, show only describe blocks.
Returns a cons cell (test-name . test-point)."
  (if-let* ((root-node (treesit-buffer-root-node 'typescript))
            (test-tree (jest-ts/map--describe-nodes root-node 0))
            (candidates (jest-ts/flatten--test-tree test-tree))
            (filtered-candidates (if describe-only
                                     (-filter (lambda (c) (s-contains? "[📘]" (nth 0 c)))
                                              candidates)
                                   candidates))
            (chosen (completing-read "Choose test: "
                                     (mapcar (lambda (c) (nth 0 c)) filtered-candidates)
                                     nil t))
            (selected (-find (lambda (c) (string= (nth 0 c) chosen)) filtered-candidates)))
      (cons (nth 1 selected) (nth 2 selected))
    (error "No tests definition found in file")))

(defun get-nodes-by-type (node type)
  "Get child nodes of NODE with specified TYPE.
NODE is a treesit node to search within.
TYPE is a string representing the node type to find.
Returns a list of matching child nodes."
  (-filter (lambda (child) (equal (treesit-node-type child) type))
           (treesit-node-children node)))

(defun get-first-node-by-type (node type)
  "Get first child node of NODE with specified TYPE.
NODE is a treesit node to search within.
TYPE is a string representing the node type to find.
Returns the first matching child node or nil if none found."
  (-find (lambda (child) (equal (treesit-node-type child) type))
         (treesit-node-children node)))

(defun get-node-text (node)
  "Extract text from a describe/test NODE.
Handles different types of string nodes:
 (string literals, binary expressions, template strings).
Returns the extracted string content."
  (let* ((args (cadr (treesit-node-children node)))
         (string-node (get-first-node-by-type args "string"))
         (binary-node (get-first-node-by-type args "binary_expression"))
         (template-node (get-first-node-by-type args "template_string")))
    (cond (string-node (substring-no-properties
                        (treesit-node-text (get-first-node-by-type string-node "string_fragment"))))
          (binary-node (binary--exp-to-string binary-node))
          (template-node (template--node-to-string template-node))
          (t (error "Bad node type: %s" (treesit-node-text args))))))

(defun get-body-node (node)
  "Get the body node from a describe/test NODE.
Extracts the statement block from the arrow function in the node.
Returns the statement block node or nil if not found."
  (let* ((args (cadr (treesit-node-children node)))
         (arrow-function (get-first-node-by-type args "arrow_function")))
    (get-first-node-by-type arrow-function "statement_block")))

(defun jest-ts/get--call-expressions (root-node call-type)
  "Get all CALL-TYPE nodes from ROOT-NODE.
ROOT-NODE is a treesit node to search within.
CALL-TYPE is a string like \"describe\", \"test\", or \"it\".
Returns a list of call expression nodes matching CALL-TYPE."
  (when-let* ((expression-statements (get-nodes-by-type root-node "expression_statement"))
              (call-expressions (-flatten
                                 (mapcar (lambda (expr)
                                           (get-nodes-by-type expr "call_expression"))
                                         expression-statements))))
    (-filter (lambda (call-exp)
               (equal (treesit-node-text
                       (car (treesit-node-children call-exp)))
                      call-type))
             call-expressions)))

(defun jest-ts/map--describe-nodes (root-node level)
  "Map describe and test nodes in ROOT-NODE at specified LEVEL.
Returns a tree structure representing the hierarchy of
describe and test blocks.
Each node in the tree contains type (describe or test),
name, position, and body for describe blocks."
  (let* ((describe-expressions (jest-ts/get--call-expressions root-node "describe"))
         (all-expressions
          (->> (append describe-expressions
                       (-filter 'identity
                                (seq-concatenate 'list
                                                 (jest-ts/get--call-expressions root-node "test")
                                                 (jest-ts/get--call-expressions root-node "it"))))
               (-sort (lambda (a b)
                        ;; TODO: check this logic
                        (> (treesit-node-start a)
                           (treesit-node-start b)))))))
    (list
     (cons 'level level)
     (cons 'nodes
           (mapcar (lambda (node)
                     (let ((node-type (treesit-node-text (car (treesit-node-children node))))
                           (node-start (treesit-node-start node)))
                       (if (string= node-type "describe")
                           (list (cons 'type 'describe)
                                 (cons 'name (get-node-text node))
                                 (cons 'position node-start)
                                 (cons 'body
                                       (jest-ts/map--describe-nodes (get-body-node node)
                                                                    (1+ level))))
                         (list (cons 'type 'test)
                               (cons 'name (get-node-text node))
                               (cons 'position node-start)))))
                   all-expressions)))))

(defun jest-ts/flatten--test-tree (tree &optional prefix)
  "Convert test TREE into a flat list of (display name position) lists.
TREE is the hierarchical structure returned by `jest-ts/map--describe-nodes'.
PREFIX is an optional string to prepend to each display name.
Returns a list where each element is (display-name test-name position)."
  (let* ((level (alist-get 'level tree))
         (indent (make-string (* level 2) ?\s))
         (prefix (or prefix ""))
         (nodes (alist-get 'nodes tree))
         (result '()))
    ;; Process each node and collect results in a proper list
    (dolist (node nodes)
      (pcase (alist-get 'type node)
        ('describe
         (push (list (format "%s%s[%s] %s"
                             prefix
                             indent
                             "📘"
                             (alist-get 'name node))
                     (alist-get 'name node)
                     (alist-get 'position node))
               result)
         ;; Process child nodes if they exist
         (when-let ((body (alist-get 'body node)))
           (setq result (append (jest-ts/flatten--test-tree body prefix) result))))
        ('test
         (push (list (format "%s%s[%s] %s"
                             prefix
                             indent
                             "✅"
                             (alist-get 'name node))
                     (alist-get 'name node)
                     (alist-get 'position node))
               result))))
    ;; Return the result in the correct order
    (nreverse result)))

;;; tests
(defvar *jest-ts/test-file-ts-node*
  (-some-> (jest-ts/read--file (expand-file-name "./jest-sb.test.ts"))
    (treesit-parse-string 'typescript)))

(ert-deftest jest-ts/create-tests-tree ()
  "Return a list of candidates that represents the test suite hierarchy using indentation and prefixes for test/describe headings."
  (should (equal (jest-ts/map--describe-nodes *jest-ts/test-file-ts-node* 0)
                 '((level . 0)
                   (nodes ((type . describe)
                           (name . "Describe 1")
                           (body
                            (level . 1)
                            (nodes
                             ((type . describe)
                              (name . "Describe 1.1")
                              (body))
                             ((type . test)
                              (name . "Describe 1 test 1"))
                             ((type . test)
                              (name . "Describe 1 test 2")))))
                          ((type . describe)
                           (name . "Describe 2")
                           (body)))))))

(ert-deftest jest-ts/flatten-tests-tree ()
  "Return a flat list of candidates that represents the test suite hierarchy using indentation and prefixes for test|describe headings."
  (should (equal (jest-ts/flatten--test-tree '((level . 0)
                                               (nodes . (((type . describe)
                                                          (name . "Describe 1")
                                                          (position . 100)
                                                          (body . nil))
                                                         ((type . describe)
                                                          (name . "Describe 2")
                                                          (position . 200)
                                                          (body . nil))))))
                 '(("[📘] Describe 1" "Describe 1" 100)
                   ("[📘] Describe 2" "Describe 2" 200))))

  (should (equal (jest-ts/flatten--test-tree '((level . 0)
                                               (nodes . (((type . describe)
                                                          (name . "Describe 1")
                                                          (position . 100)
                                                          (body . ((level . 1)
                                                                   (nodes . (((type . test)
                                                                              (name . "test 1")
                                                                              (position . 150)
                                                                              (body . nil))
                                                                             ((type . test)
                                                                              (name . "test 2")
                                                                              (position . 180)
                                                                              (body . nil)))))))
                                                         ((type . describe)
                                                          (name . "Describe 2")
                                                          (position . 200)
                                                          (body . nil))))))
                 '(("[📘] Describe 1" "Describe 1" 100)
                   ("  [✅] test 1" "test 1" 150)
                   ("  [✅] test 2" "test 2" 180)
                   ("[📘] Describe 2" "Describe 2" 200)))))

(ert-deftest jest-ts/map-describe-and-flatten-tree ()
  "Return a flat list of candidates that represents the test suite hierarchy using indentation and prefixes for test|describe headings."
  (let* ((tree (jest-ts/map--describe-nodes *jest-ts/test-file-ts-node* 0))
         (flattened (jest-ts/flatten--test-tree tree))
         ;; Extract just the first two elements for comparison, ignoring positions
         (simplified (mapcar (lambda (item) (list (nth 0 item) (nth 1 item))) flattened)))
    (should (equal simplified
                   '(("[📘] Describe 1" "Describe 1")
                     ("  [📘] Describe 1.1" "Describe 1.1")
                     ("  [✅] Describe 1 test 1" "Describe 1 test 1")
                     ("  [✅] Describe 1 test 2" "Describe 1 test 2")
                     ("[📘] Describe 2" "Describe 2"))))))

;;;###autoload
(defun jest-ts/jump-to-compilation-buffer ()
  "Jump to the Jest compilation buffer if it exists.
If the buffer doesn't exist, display a message."
  (interactive)
  (let ((buffer-name (compilation-buffer-name "jest-ts/compilation" nil nil)))
    (if (get-buffer buffer-name)
        (pop-to-buffer buffer-name)
      (message "No Jest compilation buffer found"))))

;;;###autoload
(define-minor-mode jest-ts-mode
  "Minor mode for running Jest tests in TypeScript files."
  :init-value nil
  :lighter " Jest"
  :keymap `((,(kbd "C-c C-t C-p") . jest-ts/run-test-at-point)
            (,(kbd "C-c C-t C-r") . jest-ts/rerun-latest-test)
            (,(kbd "C-c C-t C-n") . jest-ts/run-tests)
            (,(kbd "C-c C-t C-j") . jest-ts/jump-to-latest-test)
            (,(kbd "C-c C-t C-b") . jest-ts/jump-to-compilation-buffer))
  :group 'jest-ts)

(provide 'jest-ts-mode)

;;; jest-ts-mode.el ends here
