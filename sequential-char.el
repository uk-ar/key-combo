(progn
  (defun sequential-char-lookup-key1 (keymap key)
    "Like lookup-key but no third arg and no numeric return value."
    (let ((res (lookup-key keymap key)))
      (if (numberp res)
	  nil
	;; else
	res)))

  (defvar sequential-char-need-undo nil)
  (defvar sequential-char-last-prefix nil)
  ;;(setq sequential-char-need-undo nil)

  (defun sequential-char-lookup-key (key)
    ;; copy from key-chord-lookup-key
    "Lookup KEY in all current key maps."
    (message "keys:%s" key)
    (let ((maps (current-minor-mode-maps))
	  res)
      (while (and maps (not res))
	(setq res (key-chord-lookup-key1 (car maps) key)
	      maps (cdr maps)))
      (or res
	  (if (current-local-map)
	      (key-chord-lookup-key1 (current-local-map) key))
	  (key-chord-lookup-key1 (current-global-map) key))))

  (defun sequential-char()
    (interactive)
    (message "in")
    ;;(message "%s %s %s"last-command this-command sequential-char-need-undo)
    (if (and (eq real-last-command this-command)
	     sequential-char-need-undo
	     (eq last-command-event sequential-char-last-prefix)
	     (sequential-char-lookup-key
	      (vector 'sequential-char
		      (intern(string last-command-event last-command-event)))))
	(progn
	  (flet ((message (format &rest args) (identity args)))
	    (undo))
	  ;;(message "1 undo")
	  (setq sequential-char-need-undo nil)
	  (undo-boundary)
	  ))
    (let ((next-char nil)
	  command
	  (same-key t))
      (while
	  (setq command (sequential-char-lookup-key
			 ;;(current-global-map)
			 (vector 'sequential-char
				 (intern (this-command-keys)))))
	(and next-char
	     (flet ((message (format &rest args) (identity args)))
		      (undo));;(message "2 undo")
	      (undo-boundary))
	;; (message "lc:%s lce:%c tck:%s lcc:%c lie:%c lef:%s"
	;; 	      last-command last-command-event
	;; 	      (this-command-keys)
	;; 	      last-command-char
	;; 	      last-input-event
	;; 	      last-event-frame
	;; 	      )
	;;(message "l2:%c" last-input-event)

	(cond ((and (stringp command)
		    (string-match "`!!'" command))
	       (destructuring-bind (pre post)(split-string command "`!!'")
		 (insert pre)
		 (save-excursion (insert post))))
	      ((stringp command)
	       (insert command))
	      (t (command-execute command)))
	(undo-boundary)	
	(setq sequential-char-last-prefix last-input-event
	      same-key (and same-key(eq last-input-event last-command-event))
	      sequential-char-need-undo same-key
	      next-char (read-event))
	;;(message "l1:%c" last-input-event)
	)
      (and next-char
	   (setq unread-command-events (cons next-char unread-command-events)))
      )
    ;;(message "l0:%c" last-input-event)
    )

  (defun sequential-char-define (keymap keys commands)
    "Define in KEYMAP, a sequential-char of two keys in KEYS starting a COMMAND.
\nKEYS can be a string or a vector of two elements. Currently only elements
that corresponds to ascii codes in the range 32 to 126 can be used.
\nCOMMAND can be an interactive function, a string, or nil.
If COMMAND is nil, the sequential-char is removed."
    ;;copy from key-chord-define
    (if (and(listp commands) (not (eq commands nil)))
	(let ((key keys))
	  (mapc '(lambda(command)
		   (sequential-char-define keymap keys command)
		   (setq keys (concat keys key))
		   )commands))
      (let* ((key1 (substring keys 0 1))
	     (command (sequential-char-lookup-key key1))
	     )
	(if (not (eq command 'sequential-char))
	    (progn
	      ;;(message "ng")
	      (define-key keymap key1 'sequential-char)
	      )
	  ;;(lookup-key )
	  )
	;;(message "%s" commands)
	(define-key keymap (vector 'sequential-char (intern keys)) commands)
	))
    )
  
  (defun sequential-char-define-global (keys command)
    "Define a sequential-char of two keys in KEYS starting a COMMAND.
\nKEYS can be a string or a vector of two elements. Currently only elements
that corresponds to ascii codes in the range 32 to 126 can be used.
\nCOMMAND can be an interactive function, a string, or nil.
If COMMAND is nil, the sequential-char is removed."
    ;;(interactive "sSet key chord globally (2 keys): \nCSet chord \"%s\" to command: ")
    (sequential-char-define (current-global-map) keys command))
  )

(global-set-key (kbd "=") 'sequential-char)
;; (global-set-key (kbd "=") 'seq-ng)
;;(sequential-char-define-global (kbd "=") " = ")
(sequential-char-define-global (kbd "=") "(=`!!')")
(sequential-char-define-global (kbd "=") '(" = " " == " "="))
(sequential-char-define-global (kbd "=>") " => ")
(sequential-char-define-global (kbd "(") 'skeleton-pair-insert-maybe)
(sequential-char-define-global (kbd "(=") "(=`!!')")

;;(global-set-key (kbd "==") 'sequential-char) => ng
;; (global-set-key (kbd "=") 'seq-ng)
;;(sequential-char-define-global (kbd "=") " = ")
;; 1つならサイクルなし

;;skeleton(())後ろがかっこの場合囲む

;;ng
;;(sequential-char-define-global (kbd "(=") "(=")
;;char-define (vector 'sequential-char (intern keys)) 'self-insert-command)

(sequential-char-define-global (kbd "_") '(" _ " " __ " ))
(sequential-char-define-global (kbd ">") '(" > " " >> " ))
(sequential-char-define-global (kbd "-") '(" - " " -- " ))
(sequential-char-define-global (kbd "->") " -> ")

;;(sequential-char-define-global (kbd "=") '(" a " " aa "))
(sequential-char-define-global (kbd "=") '(nil nil nil))
(sequential-char-define-global (kbd "-") '(nil nil))
(global-set-key(kbd "-") 'self-insert-command)

;;(sequential-char-define-global (kbd "==") " == ")
;;(sequential-char-define-global (kbd "===") "=")


;; (define-key (current-global-map) (vector 'sequential-char (intern "=")) " = ")
;; (define-key (current-global-map) (vector 'sequential-char (intern "==")) " == ")
;; (define-key (current-global-map) (vector 'sequential-char (intern "=>")) " => ")

;; (define-key (current-global-map) (vector 'sequential-char (intern "bb")) 'hoge)
;; (lookup-key (current-global-map) (vector 'sequential-char (intern "==")))

;; (global-set-key (kbd "=") '(lambda()
;; 			     (buffer-disable-undo)
;; 			     (buffer-enable-undo)
;; 			     (interactive)
;; 			     ;;(undo-boundary)
;; 			     ;;(save-excursion
;; 			     (undo-boundary)
;; 			     (insert "abc")
;; 			     (undo-boundary)
;; 			     ;;(delete-backward-char 3)
;; 			     (primitive-undo 2 buffer-undo-list)
;; 			     ;;(undo)
;; 			     ))
;; buffer-undo-list

;; ok
;; (nil (#("abc" 0 3 (fontified nil)) . -5835) nil (5835 . 5838) (t 19860 . 19510))
;; Undo!
;; (nil (5835 . 5838) (t 19860 . 19516) nil (#("abc" 0 3 (fontified nil)) . -5835) nil (5835 . 5838) (t 19860 . 19510))
((string-match smartchr-template-cursor-re template)
 (destructuring-bind (pre post) (split-string template smartchr-template-cursor-re)
   (lexical-let ((pre pre) (post post))
     (smartchr-make-struct
      :cleanup-fn (lambda ()
		    (delete-backward-char (length pre))
		    (delete-backward-char (- (length post))))
      :insert-fn (lambda ()
		   (insert pre)
		   (save-excursion (insert post)))))))
(t
 (lexical-let ((template template))
   (smartchr-make-struct
    :cleanup-fn (lambda () (delete-backward-char (length template)))
    :insert-fn (lambda () (insert template)))))))
