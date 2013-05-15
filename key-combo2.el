
;; (defun key-combo-on ()
;;   ;;(key-combo-prefix-mode 1)
;;   (setq key-combo-prefix-mode t)
;;   (remove-hook 'pre-command-hook 'key-combo-on t))
;; (put 'key-combo-on 'permanent-local-hook t)

;; (defun key-combo-command-end ()
;;   ;; turn off the emulation layer but reactivate it at the next command
;;   ;;(key-combo-prefix-mode -1)
;;   (setq key-combo-prefix-mode nil)
;;   (add-hook 'pre-command-hook 'key-combo-on nil t)
;;   ;; restore last command
;;   (setq this-command last-command)
;;   ;; pass prefix argument
;;   (setq prefix-arg current-prefix-arg)
;;   ;; reset this-command-keys
;;   (reset-this-command-lengths))

(defun key-combo-test-helper-execute (cmd)
  ;; (key-combo-mode 1)
  (execute-kbd-macro (key-combo-read-kbd-macro cmd))
  (substring-no-properties (buffer-string)))

(define-key global-map
  (key-combo-make-key-vector (kbd "M-a a"))
  (lambda ()
    (interactive)
    (insert "kc[M-a a]")))

(define-key global-map
  (key-combo-make-key-vector (kbd "M-a"))
  (lambda ()
    (interactive)
    (insert "kc[M-a]")
    ))

(local-set-key
  (key-combo-make-key-vector (kbd ";"))
  (lambda ()
    (interactive)
    (insert ";; ")
    ;;(message "kc[M-a]")
    )
  )

(define-prefix-command 'test-map)
(global-set-key (kbd "M-a") 'test-map)
(define-key test-map (kbd "b")
  (lambda ()
    (interactive)
    (insert "n[M-a b]")))

;; can not use recent-keys because it does't record keyborad macro
;; (defvar my-key-combo-count 0)
(defun my-key-combo-keys-vector ()
  ;; (read-kbd-macro (substring (symbol-name (intern "_M-a a")) 1)))
  (vconcat (read-kbd-macro (substring (symbol-name
                              (if (symbolp last-command-event)last-command-event)
                              ) 1))))

(defun my-unread-events (vector)
  ;;cannot use push because need to concat vector and list
  (setq unread-command-events
        (append vector
                unread-command-events))
  ;;(reset-this-command-lengths)
  )

(defadvice my-post-command (around intercept activate)
  (condition-case err
      ad-do-it
    ;; Let the debugger run
    ((debug error) (signal (car err) (cdr err)))))

(defun key-combo-execute-original ()
  (interactive)
  ;; for self-insert-command
  (setq last-command-event (aref (my-key-combo-keys-vector)0))
  (call-interactively (key-binding (vector last-command-event)))
  )

(setq debug-on-error t)
;; (progn
;;   (setq last-command-event ? )
;;   ;; (self-insert-command 1)
;;   (call-interactively 'self-insert-command)
;;   )

(defun my-post-command ()
  ;;(message "this:%S" (this-command-keys-vector))
  ;; (message "this:%S" last-nonmenu-event)
  ;; (message "this:%s" last-command-event)
  ;; (message "this:%S" last-nonmenu-event)
  ;; (message "1 thi:%s li:%s ln:%s lc:%s"
  ;;          (key-description (this-command-keys-vector))
  ;;          (key-description (vector last-input-event))
  ;;          (key-description (vector last-nonmenu-event))
  ;;          (key-description (vector last-command-event))
  ;;          )
  (let* ((in-key-combo (eq 'key-combo
                           (if (< 0 (length (this-command-keys-vector)))
                               (aref (this-command-keys-vector) 0))))
         (keys-vector (if in-key-combo (my-key-combo-keys-vector) nil));
         (events (vector (read-event))))
    ;; (message "1.5 thi:%s rev:%S ev:%s li:%S lc:%S ln:%S vck:%s in:%s vc:%s"
    ;;          (key-description (this-command-keys-vector))
    ;;          events
    ;;          (if (equal events [-1]) "-1" (key-description events))
    ;;          last-input-event
    ;;          last-command-event
    ;;          last-nonmenu-event
    ;;          (if (equal events [-1]) "-1"
    ;;            (key-description (vconcat keys-vector events)))
    ;;          in-key-combo
    ;;          (vconcat keys-vector events))
    (cond
     ;; already start
     ((and in-key-combo (key-combo-key-binding (vconcat keys-vector events)))
      ;; keep
      (primitive-undo 1 buffer-undo-list)
      (my-unread-events (key-combo-make-key-vector (vconcat keys-vector events)))
      )
     ;; loop
     ((and in-key-combo
           (not (key-combo-key-binding (vconcat keys-vector events)))
           ;; for 1 key eg.<key-combo> SPC SPC
           (not (eq (length (vconcat keys-vector events)) 3))
           (key-combo-key-binding events))
      ;; finish
      (primitive-undo 1 buffer-undo-list)
      (my-unread-events (key-combo-make-key-vector events))
      )
     ((key-combo-key-binding events)
      ;; start
      (my-unread-events (key-combo-make-key-vector events))
      )
     ((and in-key-combo
           (not (key-combo-key-binding (vconcat keys-vector events)))
           (key-binding (vconcat keys-vector events)))
      ;; finish
      ;; fall back prefix
      ;; Todo: multiple prefix
      (primitive-undo 1 buffer-undo-list)
      (my-unread-events (vconcat keys-vector events))
      )
     (t
      ;; no key combo
      (my-unread-events events)
      )
     )
    ;; (message "2 thi:%s rev:%S ev:%s li:%S lc:%S"
    ;;          (key-description (this-command-keys-vector))
    ;;          events
    ;;          (if (equal events [-1]) "-1" (key-description events))
    ;;          last-input-event
    ;;          last-command-event)
    ;;(my-unread-events events)
    (reset-this-command-lengths)
    ;;(vconcat keys-vector events)
    )
  )

(progn
  (message "start")
  ;;(execute-kbd-macro (kbd "M-a b"))
  ;;(execute-kbd-macro (key-combo-make-key-vector (kbd "M-a")))
  ;;(key-combo-test-helper-execute ";")
  ;;(execute-kbd-macro (key-combo-make-key-vector (kbd ";")))
  ;;(my-unread-events (key-combo-make-key-vector (kbd ";")))
  ;;(execute-kbd-macro (kbd ";"))
  ;;(reset-this-command-lengths)
  (message "end")
  )
;;(key-binding (key-combo-make-key-vector (kbd ";")))

;; (progn
;;   (message "start")
;;   (my-unread-events (kbd "M-a b"))
;;   ;;(execute-kbd-macro (key-combo-make-key-vector (kbd "M-a")))
;;   (message "end")
;;   )
(add-hook 'post-command-hook #'my-post-command t)
;; (add-hook 'post-command-hook #'my-post-command t t)

;; (setq unread-command-events
;;       (append (key-combo-make-key-vector (kbd "M-a a"))
;;               unread-command-events))

;; (remove-hook 'post-command-hook #'my-post-command)
;; (remove-hook 'post-command-hook #'my-post-command t)

(global-key-combo-mode -1)
(key-combo-mode -1)

(dont-compile
  (when (fboundp 'describe)
    (describe ("key-combo in temp-buffer" :vars ((mode)))
      (around
        ;; (setq key-combo-command-keys nil)
        (with-temp-buffer
          (switch-to-buffer (current-buffer))
          (buffer-enable-undo)
          (emacs-lisp-mode)
          (let (;; (key-combo-prefix-mode-map (make-sparse-keymap))
                ;; (key-combo-prefix-mode-map-alist nil)
                (global-map-org (current-global-map))
                (global-map
                 (let ((map (make-sparse-keymap)))
                   (set-keymap-parent map (current-global-map))
                   map)))
            (unwind-protect
                (progn
                  (use-global-map global-map)
                  (funcall el-spec:example))
              (use-global-map global-map-org)))))
      (it ()
        (should (eq key-combo-mode nil)))
      (it ()
        (should (string= (key-combo-test-helper-execute "=") "= ")))
      (it ()
        (should (string= (key-combo-test-helper-execute "SPC") " ")))
      (it ()
        (should (string= (key-combo-test-helper-execute "= =") "eq ")))
      (it ()
        (should (string= (key-combo-test-helper-execute "= = =") "equal ")))
      (it ()
        (should (string= (key-combo-test-helper-execute "= = = =") "= ")))
      (it ()
        (should (string= (key-combo-test-helper-execute "M-a a") "kc[M-a a]")))
      (it ()
        ;; (should (string= (key-combo-test-helper-execute "M-a") "kc[M-a]"))
        ;; (should (string= (key-combo-test-helper-execute "a") "kc[M-a a]"))
        )
      (it ()
        (should (string= (key-combo-test-helper-execute "M-a") "kc[M-a]")))
      ;; "kc[M-a]b"
      (it ()
        (should (string= (key-combo-test-helper-execute "M-a b") "n[M-a b]")))
      (it ()
        (should (string= (key-combo-test-helper-execute ";") ";; ")))
      (it ()
        (should (eq (car (key-binding (key-combo-make-key-vector (kbd ";"))))
                    'lambda)))
      )))

;; (recursive-edit)
