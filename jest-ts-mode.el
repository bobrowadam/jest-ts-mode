;;; jest-ts-mode.el --- summary -*- lexical-binding: t -*-

;; Author: Adam Bobrow
;; Maintainer: Adam Bobrow
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1")
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

;;; Code:
(require 'dash)
(require 'treesit)

(defun jest-ts/read--file (file-name)
  "Return the contents of FILE-NAME as a lisp data type."
  (when (file-exists-p file-name)
   (with-temp-buffer
     (insert-file-contents file-name)
     (buffer-string))))

(defvar *latest-test* nil)
(defcustom jest-ts-mode/jest-command-pattern
  "IN_MEMORY_DB=true node --inspect=%s ~/source/grain/node_modules/.bin/jest --config %sjest.config.ts %s %s"
  "The command template to execute for running Jest with profiling")

;;;###autoload
(defun jest-ts-mode/run-tests (describe-only)
  "Run a specific test from the current file."
  (interactive "P")
  (let ((default-directory (jest-ts-mode/find--jest-config-parent-directory))
        (test-name (jest-ts-mode/choose--test-with-completion describe-only))
        (test-file-name (buffer-file-name)))
    (progn (setq *latest-test* (list test-file-name test-name default-directory))
           (compile (jest-ts-mode/test--command
                     default-directory
                     `(:file-name ,test-file-name :test-name ,test-name))
                    'jest-ts-mode/compilation-mode))))

;;;###autoload
(defun jest-ts-mode/rerun-latest-test ()
  "Run the latest test when it exists."
  (interactive)
  (if-let ((*latest-test*)
           (default-directory (nth 3 *latest-test*))
           (file-name (car *latest-test*))
           (test-name (nth 1 *latest-test*)))
      (compile (jest-ts-mode/test--command default-directory
                                           (list :file-name
                                                 file-name
                                                 :test-name
                                                 test-name))
               'jest-ts-mode/compilation-mode)
    (error "Could not rerun latest test.
 *latest-test*: %s
 default directory: %s"
           *latest-test*
           default-directory)))

(defun jest-ts-mode/jump-to-latest-test ()
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

(defun jest-ts-mode/find--jest-config-parent-directory ()
    (or (locate-dominating-file "./" "jest.config.ts")
        (error "No jest-config found. default directory: %s"
               default-directory)))

;;;###autoload
(defun jest-ts-mode/run-test-at-point ()
  "Run the enclosing test around point."
  (interactive)
  (if-let ((default-directory (jest-ts-mode/find--jest-config-parent-directory))
           (test-name-and-point (jest-ts-mode/get--current-test-name-and-point))
           (test-file-name (buffer-file-name)))
    (progn (setq *latest-test*
                 (list test-file-name
                       (car test-name-and-point)
                       (cadr test-name-and-point)
                       default-directory))
           (compile (jest-ts-mode/test--command default-directory
                                                (list :file-name
                                                      test-file-name
                                                      :test-name 
                                                      (car test-name-and-point) 
                                                      :test-point
                                                      (cadr test-name-and-point) ))
                    'jest-ts-mode/compilation-mode))
    (error "Could not run test at point:
test-name: %s
test-file-name %s" test-name-and-point test-file-name)))

(define-compilation-mode jest-ts-mode/compilation-mode "Jest Compilation"
  "Compilation mode for Jest output."
  (add-hook 'compilation-filter-hook 'jest-ts-mode/test-colorize-compilation-buffer nil t))

(defun jest-ts-mode/test-colorize-compilation-buffer ()
  "Colorize the compilation buffer."
  (ansi-color-apply-on-region compilation-filter-start (point)))

(defun jest-ts-mode/test--command (jest-config-dir &optional test-file-name-and-pattern)
  "Create the command to run Jest tests.
TEST-FILE-NAME-AND-PATTERN is a plist with optional
`:file-name` and `:test-name`."
  (let ((file-name (or (plist-get test-file-name-and-pattern :file-name) ""))
        (test-name (plist-get test-file-name-and-pattern :test-name)))
    (->>
     (format jest-ts-mode/jest-command-pattern
             9229
             jest-config-dir
             file-name
             (if test-name
                 (format "-t \"%s\""
                         test-name)
               ""))
     s-trim-right
     (s-replace-regexp "\\[\\|\\]" "\\\\\\&"))))

(defun jest-ts-mode/is--jest-test-call (node)
  "Check if the given NODE is a Jest test function (describe | it | test)."
  (and (string= (treesit-node-type node) "call_expression")
       (let* ((function-node (treesit-node-child-by-field-name node "function"))
              (function-name (treesit-node-text function-node t)))
         (member function-name '("describe" "it" "test")))))

(defun jest-ts-mode/get--current-test-name-and-point ()
  "Extract the current test name using Treesit."
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (when (treesit-ready-p 'typescript)
      (-if-let* ((node (treesit-node-at (point)))
                 (test-node (treesit-parent-until node #'jest-ts-mode/is--jest-test-call))
                 (test-name-node (treesit-node-child-by-field-name test-node "arguments"))
                 (first-arg-node (treesit-node-child test-name-node 1))
                 (node-type (treesit-node-type first-arg-node))
                 (node-start-point (treesit-node-start test-node)))
          (list (string-node-string-fragment first-arg-node)
                node-start-point)))))

(defun string-node-string-fragment (node)
  (cond 
   ((string= (treesit-node-type node) "string")
    (treesit-node-text (nth 1 (treesit-node-children node)) t))
   ((string= (treesit-node-type node) "binary_expression") (binary--exp-to-string node))
   (t (error "bad string node type"))))

(defun binary--exp-to-string (node)
  (mapconcat (lambda (child)
               (substring-no-properties
                (treesit-node-text
                 (get-first-node-by-type child "string_fragment"))))
             (treesit-node-children node "arguments")))

(defun template--node-to-string (node)
  (mapconcat #'treesit-node-text
             (treesit-node-children node "arguments")))

(defun jest-ts-mode/choose--test-with-completion (&optional describe-only)
  "Choose a test using completion.
If DESCRIBE-ONLY is non-nil, show only describe blocks."
  (if-let* ((root-node (treesit-buffer-root-node 'typescript))
            (test-tree (jest-ts/map--describe-nodes root-node 0))
            (candidates (jest-ts/flatten--test-tree test-tree))
            (filtered-candidates (if describe-only
                                     (-filter (lambda (c) (s-contains? "[📘]" (car c)))
                                              candidates)
                                   candidates))
            (chosen (completing-read "Choose test: "
                                     (mapcar #'car filtered-candidates)
                                     nil t)))
      (cdr (assoc chosen filtered-candidates))
    (error "No tests definition found in file")))

(defun get-nodes-by-type (node type)
  "Get child nodes of NODE with specified TYPE."
  (-filter (lambda (child) (equal (treesit-node-type child) type))
           (treesit-node-children node)))

(defun get-first-node-by-type (node type)
  "Get first child node of NODE with specified TYPE."
  (-find (lambda (child) (equal (treesit-node-type child) type))
         (treesit-node-children node)))

(defun get-node-text (node)
  "Extract text from a describe/test node."
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
  "Get the body node from a describe/test node."
  (let* ((args (cadr (treesit-node-children node)))
         (arrow-function (get-first-node-by-type args "arrow_function")))
    (get-first-node-by-type arrow-function "statement_block")))

(defun jest-ts/get--call-expressions (root-node call-type)
  "Get all CALL-TYPE nodes from ROOT-NODE."
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
  "Map describe and test nodes in ROOT-NODE at specified LEVEL."
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
                     (let ((node-type (treesit-node-text (car (treesit-node-children node)))))
                       (if (string= node-type "describe")
                           (list (cons 'type 'describe)
                                 (cons 'name (get-node-text node))
                                 (cons 'body
                                       (jest-ts/map--describe-nodes (get-body-node node)
                                                                    (1+ level))))
                         (list (cons 'type 'test)
                               (cons 'name (get-node-text node))))))
                   all-expressions)))))

(defun jest-ts/flatten--test-tree (tree &optional prefix)
  "Convert TEST-TREE into a flat list of (display . name) cons cells."
  (let* ((level (alist-get 'level tree))
         (indent (make-string (* level 2) ?\s))
         (prefix (or prefix ""))
         (nodes (alist-get 'nodes tree)))
    (-flatten (mapcar (lambda (node)
                        (pcase (alist-get 'type node)
                          ('describe (if-let ((body (alist-get 'body node)))
                                         (list (cons (format "%s%s[%s] %s"
                                                             prefix
                                                             indent
                                                             "📘"
                                                             (alist-get 'name node))
                                                     (alist-get 'name node))
                                               (jest-ts/flatten--test-tree body prefix))
                                       (cons (format "%s%s[%s] %s"
                                                     prefix
                                                     indent
                                                     "📘"
                                                     (alist-get 'name node))
                                             (alist-get 'name node))))
                          ('test (cons (format "%s%s[%s] %s"
                                               prefix
                                               indent
                                               "✅"
                                               (alist-get 'name node))
                                       (alist-get 'name node)))))
                      nodes))))

;;; tests
(defvar *jest-ts/test-file-ts-node*
  (-some-> (jest-ts/read--file (expand-file-name "./jest-sb.test.ts"))
    (treesit-parse-string 'typescript)))

(defvar *jest-ts/test-file-ts-node-2*
  (-some-> (jest-ts/read--file (expand-file-name "./jest-sb-2.test.ts"))
    (treesit-parse-string 'typescript)))

(ert-deftest jest-ts/create-tests-tree ()
  "Return a list of candidates that represents the test suite hierarchy using indentation and prefixes for test/describe headings"
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
  "Return a flat list of candidates that represents the test suite hierarchy using indentation and prefixes for test|describe headings"
  (should (equal (jest-ts/flatten--test-tree '((level . 0)
                                               (nodes . (((type . describe)
                                                          (name . "Describe 1")
                                                          (body . nil))
                                                         ((type . describe)
                                                          (name . "Describe 2")
                                                          (body . nil))))))
                 '(("[📘] Describe 1" . "Describe 1")
                   ("[📘] Describe 2" . "Describe 2"))))

  (should (equal (jest-ts/flatten--test-tree '((level . 0)
                                               (nodes . (((type . describe)
                                                          (name . "Describe 1")
                                                          (body . ((level . 1)
                                                                   (nodes . (((type . test)
                                                                              (name . "test 1")
                                                                              (body . nil))
                                                                             ((type . test)
                                                                              (name . "test 2")
                                                                              (body . nil)))))))
                                                         ((type . describe)
                                                          (name . "Describe 2")
                                                          (body . nil))))))
                 '(("[📘] Describe 1" . "Describe 1")
                   ("  [✅] test 1" . "test 1")
                   ("  [✅] test 2" . "test 2")
                   ("[📘] Describe 2" . "Describe 2")))))

(ert-deftest jest-ts/map-describe-and-flatten-tree ()
  "Return a flat list of candidates that represents the test suite hierarchy using indentation and prefixes for test|describe headings"
  (should (equal (jest-ts/flatten--test-tree (jest-ts/map--describe-nodes *jest-ts/test-file-ts-node* 0))
                 '(("[📘] Describe 1" . "Describe 1")
                   ("  [📘] Describe 1.1" . "Describe 1.1")
                   ("  [✅] Describe 1 test 1" . "Describe 1 test 1")
                   ("  [✅] Describe 1 test 2" . "Describe 1 test 2")
                   ("[📘] Describe 2" . "Describe 2")))))


(provide 'jest-ts-mode)

;;; jest-ts-mode.el ends here
