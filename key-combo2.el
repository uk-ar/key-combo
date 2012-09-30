
;; (defun hoge ()
;;   (interactive)
;;   ;;(undo-boundary)
;;   ;; (call-interactively
;;   ;;  (key-binding (key-combo-make-key-vector (this-command-keys-vector))))
;;   (insert "hoge")
;;   ;;(call-interactively ())
;;   ;; key-combo-read for test
;;   ;;(push (listify-key-sequence [key-combo-read]) unread-command-events)
;;   (let ((event (read-event)))
;;     (if (not (eq event ?a))
;;         ;; not match key-combo
;;         (progn
;;           (setq unread-command-events
;;                 (append (listify-key-sequence
;;                          (this-command-keys-vector))
;;                         unread-command-events))
;;           (key-combo-command-end))
;;       (progn
;;         ;;(undo)
;;         (primitive-undo 1 buffer-undo-list)
;;         (setq unread-command-events
;;               (append (key-combo-make-key-vector (kbd "M-a a"))
;;                       unread-command-events))
;;         ))))

;; (defun key-combo-read ()
;;   (interactive)
;;   (message "r:aaa")
;;   (let ((event (read-event "r:")))
;;     (if (not (eq event ?a))
;;         ;; not match key-combo
;;         (progn
;;           (setq unread-command-events
;;                 (append (listify-key-sequence
;;                          (this-command-keys-vector))
;;                         unread-command-events))
;;           (key-combo-command-end))
;;       (progn
;;         (undo)
;;         ;(primitive-undo 1 buffer-undo-list)
;;         (setq unread-command-events
;;               (append (key-combo-make-key-vector (kbd "M-a a"))
;;                       unread-command-events))
;;         ))))

;; (define-prefix-command 'test-map)
;; (global-set-key (kbd "M-a") 'test-map)
;; (define-key test-map (kbd "b")
;;   (lambda ()
;;     (interactive)
;;     (insert "M-a b")))

;; (define-key key-combo-prefix-mode-map (kbd "M-a") 'hoge)
;; (define-key global-map
;;   (key-combo-make-key-vector (kbd "M-a"))
;;   (lambda ()
;;     (interactive)
;;     (insert "kc[M-a]")))
;; (define-key global-map
;;   (key-combo-make-key-vector (kbd "M-a a"))
;;   (lambda ()
;;     (interactive)
;;     (insert "kc[M-a a]")))
;; (define-key global-map [key-combo-read] 'key-combo-read)

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

;; (defun hyperify (prompt)
;;   (let ((e (read-event)))
;;     ;;(vector
;;     (if (eq e ?a)
;;        (key-combo-make-key-vector (kbd "M-a a"))
;;        (vector e)
;;        ;; (append (key-combo-make-key-vector (kbd "M-a a"))
;;          ;;         unread-command-events)
;;        ;; (logior (lsh 1 24) e)
;;        ;; (if (memq 'hyper (event-modifiers e))
;;        ;;     e
;;        ;;   (append (key-combo-make-key-vector (kbd "M-a a"))
;;        ;;           unread-command-events)
;;          ;; (add-event-modifier "H-" e)
;;        ))););;)

;; (defun add-event-modifier (string e)
;;   (let ((symbol (if (symbolp e) e (car e))))
;;     (setq symbol (intern (concat string
;;                                  (symbol-name symbol))))
;;     (if (symbolp e)
;;         symbol
;;       (cons symbol (cdr e)))))

;; (define-key function-key-map (kbd "M-a") 'hyperify)
;; (define-key key-translation-map (kbd "M-a") 'hyperify)

;; (hoge)
;; assert pre-command-hook
;; assert last-command
;; assert clean-up
;; (defun hoge-1 ()
;;   (interactive)
;;   (insert "hoge"))

;; (defun hoge2 ()
;;   (interactive)
;;   ;; (insert "hoge")
;;   ;; (undo-boundary)
;;   (call-interactively 'hoge-1)
;;   (let ((event (read-event)))
;;     ;; (keyboard-quit)
;;     (if (not (eq event ?a))
;;         (setq unread-command-events
;;               (listify-key-sequence (list event)))
;;       (primitive-undo 1 buffer-undo-list)
;;       (setq unread-command-events
;;             (append (key-combo-make-key-vector (kbd "M-a a"))
;;                     unread-command-events))
;;       ;; (setq unread-command-events
;;       ;;       (listify-key-sequence (vector 'key-combo event)))
;;       )))

;; (global-set-key (kbd "M-a") 'hoge2)
;; (define-key global-map
;;   (key-combo-make-key-vector (kbd "M-a a"))
;;   (lambda ()
;;     (interactive)
;;     (insert "kc[M-a a]")))

;; (with-temp-buffer
;;   (switch-to-buffer (current-buffer))
;;   (buffer-enable-undo)
;;   (execute-kbd-macro (kbd "M-a a"))
;;   (buffer-substring-no-properties (point-min) (point-max))
;;   )

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

;; (define-key global-map
;;   (key-combo-make-key-vector (kbd ";")) ";; ]")

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
  (read-kbd-macro (substring (symbol-name last-command-event) 1)))

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

;; (defun key-combo-execute-original ()
;;   (interactive)
;;   (call-interactively (key-binding (my-key-combo-keys-vector)))
;;   )

;; (defun key-combo-on ()
;;   ;;(key-combo-prefix-mode 1)
;;   (add-hook 'post-command-hook #'my-post-command t)
;;   (remove-hook 'pre-command-hook #'key-combo-on t))
;; (put 'key-combo-on 'permanent-local-hook t)

;; (defun my-print ()
;;   (interactive)
;;   (message "0 thi:%s li:%s ln:%s lc:%s"
;;            (key-description (this-command-keys-vector))
;;            (key-description (vector last-input-event))
;;            (key-description (vector last-nonmenu-event))
;;            (key-description (vector last-command-event))))

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
    ;; (message "1.5 thi:%s rev:%S ev:%s li:%S lc:%S ln:%S vc:%s in:%s"
    ;;          (key-description (this-command-keys-vector))
    ;;          events
    ;;          (if (equal events [-1]) "-1" (key-description events))
    ;;          last-input-event
    ;;          last-command-event
    ;;          last-nonmenu-event
    ;;          (if (equal events [-1]) "-1"
    ;;            (key-description (vconcat keys-vector events)))
    ;;          in-key-combo)
    (cond
     ;; cycling
     ;; ()
     ;; already start
     ((and in-key-combo (equal (vconcat keys-vector events) (kbd "M-a a")))
      ;;continue
      (primitive-undo 1 buffer-undo-list)
      (my-unread-events (key-combo-make-key-vector (kbd "M-a a")))
      )
     ((and in-key-combo (equal (vconcat keys-vector events) (kbd "M-a b")))
      ;; finish
      ;; fall back prefix
      ;; (add-hook 'pre-command-hook #'key-combo-on nil t)
      ;; (remove-hook 'post-command-hook #'my-post-command)
      (primitive-undo 1 buffer-undo-list)
      (my-unread-events (kbd "M-a b"))
      ;;(reset-this-command-lengths)
      )
     ((and in-key-combo (equal (vconcat keys-vector events)
                               (vconcat (kbd "= ="))))
      ;;finish
      (primitive-undo 1 buffer-undo-list)
      (my-unread-events (key-combo-make-key-vector (kbd "= =")))
     )
     ((and in-key-combo (equal (vconcat keys-vector events)
                               (vconcat (kbd "= = ="))))
      ;;finish
      (primitive-undo 1 buffer-undo-list)
      (my-unread-events (key-combo-make-key-vector (kbd "= = =")))
      )
     ((and in-key-combo (equal (vconcat keys-vector events)
                               (vconcat (kbd "= = = ="))))
      ;;finish
      (primitive-undo 1 buffer-undo-list)
      (my-unread-events (key-combo-make-key-vector (kbd "=")))
      )
     (in-key-combo
      ;;finish
      ;; (add-hook 'pre-command-hook #'key-combo-on nil t)
      ;; (remove-hook 'post-command-hook #'my-post-command)
      (my-unread-events events)
      )
     ;; start
     ((equal events (kbd "M-a"))
      (my-unread-events (key-combo-make-key-vector (kbd "M-a")))
      ;;(reset-this-command-lengths)
      )
     ((equal events [?\;])
      (my-unread-events (key-combo-make-key-vector (kbd ";")))
      )
     ((equal events [?=])
      (my-unread-events (key-combo-make-key-vector (kbd "=")))
      )
     (t
      ;; no key combo
      (my-unread-events events)
      ;;(reset-this-command-lengths)
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

;; (local-set-key
;;  (key-combo-make-key-vector (kbd ";"))
;;  ;;"ho"
;;  'my-print
;;  )

(progn
  (message "start")
  ;;(execute-kbd-macro (kbd "M-a b"))
  ;;(execute-kbd-macro (key-combo-make-key-vector (kbd "M-a")))
  ;;(key-combo-test-helper-execute ";")
  (execute-kbd-macro (key-combo-make-key-vector (kbd ";")))
  ;;(my-unread-events (key-combo-make-key-vector (kbd ";")))
  ;;(execute-kbd-macro (kbd ";"))
  ;;(reset-this-command-lengths)
  (message "end")
  );; 
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
        (should (string= (key-combo-test-helper-execute "= =") "eq ")))
      (it ()
        (should (string= (key-combo-test-helper-execute "= = =") "equal ")))
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
      ;; (it ()
      ;;   (should (string= (key-combo-test-helper-execute ";") ";; ")))
      (it ()
        (should (eq (car (key-binding (key-combo-make-key-vector (kbd ";"))))
                    'lambda)))
      )))

;; (recursive-edit)
