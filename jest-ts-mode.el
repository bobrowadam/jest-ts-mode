(require 'treesit)

(defvar *latest-test* nil)

;;;###autoload
(defun jest-ts-mode/run-tests (describe-only)
  "Run a specific test from the current file"
  (interactive "P")
  (if-let ((default-directory (locate-dominating-file "./" "jest.config.ts"))
           (test-name (jest-ts-mode/choose--test-with-completion describe-only))
           (test-file-name (buffer-file-name)))
      (progn (setq *latest-test* (list test-file-name test-name default-directory))
             (compile (jest-ts-mode/test--command
                       default-directory
                       `(:file-name ,test-file-name :test-name ,test-name))
                      'jest-ts-mode/compilation-mode))
    (error "No jest-config found. default directory: %s" default-directory)))

;;;###autoload
(defun jest-ts-mode/rerun-latest-test ()
  "Run the latest test when exists."
  (interactive)
  (when *latest-test*
    (if-let ((default-directory (nth 2 *latest-test*)))
        (compile (jest-ts-mode/test--command
                  default-directory
                  `(:file-name ,(car *latest-test*) :test-name ,(nth 1 *latest-test*)))
                 'jest-ts-mode/compilation-mode)
      (error "No jest-config found. default directory: %s" default-directory))))

;;;###autoload
(defun jest-ts-mode/run-test-on-point ()
  "Run the enclosing test around point"
  (interactive)
  (if-let ((default-directory (locate-dominating-file "./" "jest.config.ts"))
           (test-name (jest-ts-mode/get--current-test-name))
           (test-file-name (buffer-file-name)))
      (progn (setq *latest-test* (list test-file-name test-name default-directory))
             (compile (jest-ts-mode/test--command
                       default-directory
                       `(:file-name ,test-file-name :test-name ,test-name))
                      'jest-ts-mode/compilation-mode))
    (error "No jest-config found. default directory: %s" default-directory)))

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
    (s-trim-right (format
                   "IN_MEMORY_DB=true node --inspect=%s ~/source/grain/node_modules/.bin/jest --runInBand --detectOpenHandles --config %sjest.config.ts %s %s"
                   (bob/get-inspect-port)
                   jest-config-dir
                   file-name
                   (if test-name (format "-t \"%s\"" test-name) "")))))

(defun jest-ts-mode/is--jest-test-call (node)
  "Checks if the given NODE is a Jest test function (describe | it | test)."
  (and (string= (treesit-node-type node) "call_expression")
       (let* ((function-node (treesit-node-child-by-field-name node "function"))
              (function-name (treesit-node-text function-node t)))
         (member function-name '("describe" "it" "test")))))

(defun jest-ts-mode/get--current-test-name ()
  "Extract the current test name using Treesit."
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (when (treesit-ready-p 'typescript)
      (-if-let* ((node (treesit-node-at (point)))
                 (test-node (treesit-parent-until node #'jest-ts-mode/is--jest-test-call))
                 (test-name-node (treesit-node-child-by-field-name test-node "arguments"))
                 (first-arg-node (treesit-node-child test-name-node 1))
                 (node-type (treesit-node-type first-arg-node)))
          (when (string= node-type "string")
            (string-node-string-fragment first-arg-node))))))

(defun string-node-string-fragment (node)
  (if (string= (treesit-node-type node) "string")
      (treesit-node-text (nth 1 (treesit-node-children node)) t)
    (error "node is not of type string")))

(defun jest-ts-mode/choose--test-with-completion (&optional describe-only)
  "Choose a test using completion.
If DESCRIBE-ONLY is non-nil, show only describe blocks."
  (if-let* ((root-node (treesit-buffer-root-node))
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
         (string-fragment (get-first-node-by-type string-node "string_fragment")))
    (substring-no-properties (treesit-node-text string-fragment))))

(defun get-body-node (node)
  "Get the body node from a describe/test node."
  (let* ((args (cadr (treesit-node-children node)))
         (arrow-function (get-first-node-by-type args "arrow_function")))
    (get-first-node-by-type arrow-function "statement_block")))

(defun get-test-content (test-node)
  "Extract test expectations from TEST-NODE."
  (let* ((body-node (get-body-node test-node))
         (expressions (get-nodes-by-type body-node "expression_statement")))
    (mapcar (lambda (expr)
              (let ((call-expr (get-first-node-by-type expr "call_expression")))
                (when call-expr
                  (treesit-node-text call-expr))))
            expressions)))

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
          (->> (append describe-expressions (jest-ts/get--call-expressions root-node "test"))
               ;; Sort by start position to maintain source order
               (-sort (lambda (a b)
                        (> (treesit-node-index a)
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
         (nodes (alist-get 'nodes tree))
         result)
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

(defvar *jest-ts/test-file-ts-node* (treesit-parse-string (read-file "./jest-sb.test.ts") 'typescript))
(defvar *jest-ts/test-file-ts-node-2* (treesit-parse-string (read-file "./jest-sb-2.test.ts") 'typescript))

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
