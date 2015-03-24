;; Usage:
;;
;;   emacs -Q -l tests/run-test.el           # interactive mode
;;   emacs -batch -Q -l tests/run-test.el    # batch mode


;; Utils
(defun key-combo-test-join-path (path &rest rest)
  "Join a list of PATHS with appropriate separator (such as /).
\(fn &rest paths)"
  (if rest
      (concat (file-name-as-directory path) (apply 'key-combo-test-join-path rest))
    path))

(defvar key-combo-test-dir (file-name-directory load-file-name))
(defvar key-combo-root-dir (concat key-combo-test-dir ".."))

;; Setup `load-path'
(mapc (lambda (p) (add-to-list 'load-path p))
      (list key-combo-test-dir
            key-combo-root-dir))

;; Load tests
(load "key-combo-tests")
(ert-run-tests-batch-and-exit t)
