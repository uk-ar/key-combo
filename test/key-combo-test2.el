(require 'ert)
(require 'el-spec)
(require 'test-double)

(require 'key-combo)

(defmacro key-combo-test-helper-common (&rest body)
  (declare (indent 0) (debug t))
  `(with-mock2
     (setq key-combo-command-keys nil)
     (with-temp-buffer
       (switch-to-buffer (current-buffer))
       (let (;; (key-combo-mode-map
             ;;  (copy-keymap key-combo-mode-map))
             (key-combo-prefix-mode-map (make-sparse-keymap))
             (key-combo-prefix-mode-map-alist nil)
             (global-map-org (current-global-map))
             (global-map
              (let ((map (make-sparse-keymap)))
                (set-keymap-parent map (current-global-map))
                map)))
         (unwind-protect
             (progn
               (use-global-map global-map)
               ;;
               (key-combo-mode 1)
               ,@body
               )
           (use-global-map global-map-org))))
     ))

(defun test1 () (interactive))
(defun test2 () (interactive))

(ert-deftest prefix1 ()
  (key-combo-test-helper-common
   (should (keymapp (key-binding (kbd "M-s"))));; prefix
   (defmock test1 () (interactive))
   (defmock test2 () (interactive))

   ;; (global-set-key (kbd "M-s") 'test3);workaround
   (key-combo-define-global (kbd "M-s") 'test1)
   (key-combo-define-global (kbd "M-s a") 'test2)
   (execute-kbd-macro (kbd "M-s a"))
   (should (eq (el-spec:called-count 'test1) 1))
   (should (eq (el-spec:called-count 'test2) 1))
   ))

(ert-deftest prefix2 ()
  (key-combo-test-helper-common
    (should (keymapp (key-binding (kbd "M-s"))));; prefix
    (defmock define-key (keymap key def)
      (funcall
       (el-spec:get-original-func 'define-key)
       keymap key def))
    (defmock test1 () (interactive))
    (defmock key-combo-prefix-command () (interactive))

    (key-combo-define-global (kbd "M-s") 'test1)
    (should (eq (el-spec:called-count 'define-key) 2))
    (should (equal (mapcar #'(lambda (x) (cdr x))
                           (el-spec:args-for-call 'define-key))
                   '(([134217843]
                      key-combo-prefix-command)
                     ([key-combo _M-s]
                      test1)
                     )))
    (should (eq (key-binding (kbd "M-s"))
                'key-combo-prefix-command))
    (should (eq (lookup-key global-map [key-combo _M-s])
                'test1))
    (should (eq (el-spec:called-count 'test1) 0))

    (execute-kbd-macro (kbd "M-s"))
    (should (eq (el-spec:called-count 'key-combo-prefix-command) 0))
    (should (eq (el-spec:called-count 'test1) 1))
    ))

(ert-deftest prefix3 ()
  (key-combo-test-helper-common
    (should (keymapp (key-binding (kbd "M-s"))));; prefix
    (defmock define-key (keymap key def)
      (funcall
       (el-spec:get-original-func 'define-key)
       keymap key def))
    (defmock test1 () (interactive))

    (key-combo-define-global (kbd "M-s a") 'test1)
    (should (eq (el-spec:called-count 'define-key) 3))
    (should (equal (mapcar #'(lambda (x) (cdr x))
                           (el-spec:args-for-call 'define-key))
                   '(([134217843]
                      key-combo-prefix-command)
                     ([key-combo _M-s]
                      key-combo-execute-original)
                     ([key-combo _M-s\ a]
                      test1))))
    (should (eq (key-binding (kbd "M-s"))
                'key-combo-prefix-command))
    (should (eq (lookup-key global-map [key-combo _M-s])
                'key-combo-execute-original))
    (should (eq (el-spec:called-count 'test1) 0))

    (execute-kbd-macro (kbd "M-s a"))
    (should (eq (el-spec:called-count 'test1) 1))
    ))

(ert-deftest test-key-combo-command-keys ()
  (key-combo-test-helper-common
    (should-not key-combo-command-keys)
    (execute-kbd-macro (kbd "C-a"))
    (should (equal key-combo-command-keys (vconcat (kbd "C-a"))))
    (execute-kbd-macro (kbd "C-a"))
    (should (equal key-combo-command-keys (vconcat (kbd "C-a C-a"))))
    (execute-kbd-macro (kbd "C-a"))
    (should (equal key-combo-command-keys (vconcat (kbd "C-a C-a C-a"))))
    ;; this is for return
    (execute-kbd-macro (kbd "C-a"))
    (should (equal key-combo-command-keys (vconcat (kbd "C-a C-a C-a C-a"))))
    (execute-kbd-macro (kbd "C-a"))
    (should (equal key-combo-command-keys (vconcat (kbd "C-a"))))
    ))

(ert-deftest test-key-combo-command-keys2 ()
  (key-combo-test-helper-common
    (should-not key-combo-command-keys)
    (key-combo-define-global (kbd "M-s b") 'test1)
    (execute-kbd-macro (kbd "M-s"))
    (should (equal key-combo-command-keys (vconcat (kbd "M-s"))))
    (execute-kbd-macro (kbd "b"))
    (should (equal key-combo-command-keys (vconcat (kbd "M-s b"))))
    (execute-kbd-macro (kbd "c"))
    (should (equal key-combo-command-keys nil))
    ))

(ert-deftest test-key-combo-command-keys3 ()
  (key-combo-test-helper-common
    (global-set-key (kbd "M-s a") 'test2)
    (should (key-binding (kbd "M-s a")))
    (should-not key-combo-command-keys)
    (key-combo-define-global (kbd "M-s b") 'test1)
    (execute-kbd-macro (kbd "M-s"))
    (should (equal key-combo-command-keys (vconcat (kbd "M-s"))))
    (execute-kbd-macro (kbd "b"))
    ;; (should (eq (el-spec:called-count 'test2) 1))
    ;; (should (equal key-combo-command-keys (vconcat (kbd "M-s b"))))
    ;; (execute-kbd-macro (kbd "c"))
    ;; (should (equal key-combo-command-keys (vconcat (kbd "c"))))
    ))

(ert-deftest key-combo-check-keys10 ()
  (let ((key-combo-need-undop nil))
    (should (equal nil
                   (key-combo-check-keys (vconcat (kbd "M-s"))
                                         (vconcat (kbd "b")))))
    (should (eq key-combo-need-undop nil))
    ))

(ert-deftest prefix4 ()
  (key-combo-test-helper-common
    (should (keymapp (key-binding (kbd "M-s"))));; prefix
    (defmock test2 () (interactive))
    (global-set-key (kbd "M-s a") 'test2)
    (defmock define-key (keymap key def)
      (funcall
       (el-spec:get-original-func 'define-key)
       keymap key def))
    (defmock test1 () (interactive))
    (defmock key-combo-prefix-command () (interactive))

    (key-combo-define-global (kbd "M-s b") 'test1)
    (should (eq (el-spec:called-count 'define-key) 3))
    (should (equal (mapcar #'(lambda (x) (cdr x))
                           (el-spec:args-for-call 'define-key))
                   '(([134217843]
                      key-combo-prefix-command)
                     ([key-combo _M-s]
                      key-combo-execute-original)
                     ([key-combo _M-s\ b]
                      test1))))
    (should (eq (key-binding (kbd "M-s"))
                'key-combo-prefix-command))
    (should (eq (lookup-key global-map [key-combo _M-s])
                'key-combo-execute-original))
    (should (eq (el-spec:called-count 'test1) 0))

    (execute-kbd-macro (kbd "M-s"))
    (should (equal key-combo-command-keys (vconcat (kbd "M-s"))))
    (execute-kbd-macro (kbd "b"))
    (should (equal key-combo-command-keys (vconcat (kbd "M-s b"))))
    ;; (should (eq (el-spec:called-count 'key-combo-prefix-command) 1))
    ;; (should (eq (el-spec:called-count 'test1) 0))
    ;; (should (eq (el-spec:called-count 'test2) 1))
    ))

(ert-deftest key-combo-check-keys1 ()
  (let ((key-combo-need-undop nil))
    (should (equal (vconcat (kbd "C-a"))
                   (key-combo-check-keys nil
                                         (vconcat (kbd "C-a")))))
    (should (eq key-combo-need-undop nil))
    ))

(ert-deftest key-combo-check-keys2 ()
  (let ((key-combo-need-undop nil))
    (should (equal (vconcat (kbd "C-a C-a"))
                   (key-combo-check-keys (vconcat (kbd "C-a"))
                                         (vconcat (kbd "C-a")))))
    (should (eq key-combo-need-undop nil))
    ))

(ert-deftest key-combo-check-keys3 ()
  (let ((key-combo-need-undop nil))
    (should (equal (vconcat (kbd "C-a C-a C-a"))
                   (key-combo-check-keys (vconcat (kbd "C-a C-a"))
                                         (vconcat (kbd "C-a")))))
    (should (eq key-combo-need-undop nil))
    ))

(ert-deftest key-combo-check-keys4 ()
  (let ((key-combo-need-undop nil))
    (should (equal (vconcat (kbd "C-a C-a C-a C-a"))
                   (key-combo-check-keys (vconcat (kbd "C-a C-a C-a"))
                                         (vconcat (kbd "C-a")))))
    (should (eq key-combo-need-undop nil))
    ))

(ert-deftest key-combo-check-keys4 ()
  (let ((key-combo-need-undop nil))
    (should (equal (vconcat (kbd "C-a"))
                   (key-combo-check-keys (vconcat (kbd "C-a C-a C-a C-a"))
                                         (vconcat (kbd "C-a")))))
    (should (eq key-combo-need-undop t))
    ))

(ert-deftest key-combo-check-keys5 ()
  (let ((key-combo-need-undop nil))
    (should (equal (vconcat (kbd "C-e"))
                   (key-combo-check-keys (vconcat (kbd "C-a C-a C-a C-a"))
                                         (vconcat (kbd "C-e")))))
    (should (eq key-combo-need-undop nil))
    ))

(ert-deftest key-combo-check-keys5 ()
  (let ((key-combo-need-undop nil))
    (should (equal (vconcat (kbd "C-e"))
                   (key-combo-check-keys (vconcat (kbd "C-a"))
                                         (vconcat (kbd "C-e")))))
    (should (eq key-combo-need-undop nil))
    ))

(ert-deftest key-combo-check-keys6 ()
  (let ((key-combo-need-undop nil))
    (should (equal nil
                   (key-combo-check-keys nil
                                         (vconcat (kbd "=")))))
    (should (eq key-combo-need-undop nil))
    ))

(ert-deftest key-combo-check-keys7 ()
  (let ((key-combo-need-undop nil))
    (emacs-lisp-mode)
    (should (equal (vconcat (kbd "."))
                   (key-combo-check-keys nil
                                         (vconcat (kbd ".")))))
    (should (eq key-combo-need-undop nil))
    ))

(ert-deftest key-combo-check-keys8 ()
  (let ((key-combo-need-undop nil))
    (emacs-lisp-mode)
    (should (equal (vconcat (kbd "."))
                   (key-combo-check-keys (vconcat (kbd "."))
                                         (vconcat (kbd ".")))))
    (should (eq key-combo-need-undop nil))
    ))

(ert-deftest key-combo-check-keys9 ()
  (let ((key-combo-need-undop nil))
    (emacs-lisp-mode)
    (should (equal nil
                   (key-combo-check-keys nil
                                         (vconcat (kbd "a")))))
    (should (eq key-combo-need-undop nil))
    ))

;; (ert-deftest prefix5 ()
;;   (key-combo-test-helper-common
;;     (should (keymapp (key-binding (kbd "M-s"))));; prefix
;;     (defmock test2 () (interactive))
;;     (global-set-key (kbd "M-s a") 'test2)
;;     (defmock define-key (keymap key def)
;;       (funcall
;;        (el-spec:get-original-func 'define-key)
;;        keymap key def))
;;     (defmock test1 () (interactive))
;;     (defmock key-combo-prefix-command () (interactive))

;;     (key-combo-define-global (kbd "M-s") 'test1)
;;     (should (eq (el-spec:called-count 'define-key) 2))
;;     (should (equal (mapcar #'(lambda (x) (cdr x))
;;                            (el-spec:args-for-call 'define-key))
;;                    '(([134217843]
;;                       key-combo-prefix-command)
;;                      ([key-combo _M-s]
;;                       test1)
;;                      )))
;;     (should (eq (key-binding (kbd "M-s"))
;;                 'key-combo-prefix-command))
;;     (should (eq (lookup-key global-map [key-combo _M-s])
;;                 'test1))
;;     (should (eq (el-spec:called-count 'test1) 0))

;;     (execute-kbd-macro (kbd "M-s a"))
;;     (should (eq (el-spec:called-count 'key-combo-prefix-command) 0))
;;     (should (eq (el-spec:called-count 'test1) 1))
;;     (should (eq (el-spec:called-count 'test2) 1))
;;     ))
