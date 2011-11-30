;;; key-combo.el --- map key sequence to commands

;;-------------------------------------------------------------------
;;
;; Copyright (C) 2011 Yuuki Arisawa
;;
;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA
;;
;;-------------------------------------------------------------------

;; Author: Yuuki Arisawa <yuuki.ari@gmail.com>
;; URL:https://github.com/uk-ar/key-combo
;; Created: 30 November 2011
;; Version: 0.1
;; Keywords: keyboard input

;;; Commentary:

;; ########   Compatibility   ########################################
;;
;; Works with Emacs-23.2.1

;; ########   Quick start   ########################################
;;
;; Add to your ~/.emacs
;;
;;	(require 'key-combo)
;;
;; and some chords, for example
;;
;;      (key-chord-define-global "hj"     'undo)
;;      (key-chord-define-global ",."     "<>\C-b")

;; (require 'key-combo)
;; (key-combo-load-default)

;; Code goes here
(progn
  (defun key-combo-lookup-key1 (keymap key)
    "Like lookup-key but no third arg and no numeric return value."
    (let ((res (lookup-key keymap key)))
      (if (numberp res)
	  nil
	;; else
	res)))

  (defvar key-combo-need-undo nil)
  (defvar key-combo-last-prefix nil)
  ;;(setq key-combo-need-undo nil)

  (defun key-combo-lookup-key (key)
    ;; copy from key-chord-lookup-key
    "Lookup KEY in all current key maps."
		(message "keys:%s" key)
    (let ((maps (current-minor-mode-maps))
					res)
      (while (and maps (not res))
	(setq res (key-combo-lookup-key1 (car maps) key)
	      maps (cdr maps)))
      (or res
	  (if (current-local-map)
	      (key-combo-lookup-key1 (current-local-map) key))
	  (key-combo-lookup-key1 (current-global-map) key))))
  ;;bug (C-/
  (defun key-combo()
    (interactive)
    (message "in")
    ;;(message "%s %s %s"last-command this-command key-combo-need-undo)
    (if (and (eq real-last-command this-command)
						 key-combo-need-undo
						 (eq last-command-event key-combo-last-prefix)
						 (key-combo-lookup-key
							(vector 'key-combo
											(intern(string last-command-event last-command-event)))))
				(progn
					(flet ((message (format &rest args) (identity args)))
						(undo))														;this is for undo message.
					;;(message "1 undo")
					(setq key-combo-need-undo nil)
					(undo-boundary)
					))
    (let ((next-char nil)
					command
					(same-key t))
      (while
					(and (stringp(this-command-keys))
							 (setq command (key-combo-lookup-key
															;;(current-global-map)
															(vector 'key-combo
																			(intern (this-command-keys))))))
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
				(setq key-combo-last-prefix last-input-event
							same-key (and same-key(eq last-input-event last-command-event))
							key-combo-need-undo same-key
							next-char (read-event))
				;;(message "l1:%c" last-input-event)
				)
      (and next-char
					 (setq unread-command-events (cons next-char unread-command-events)))
      )
    ;;(message "l0:%c" last-input-event)
    )

  (defun key-combo-define (keymap keys commands)
    "Define in KEYMAP, a key-combo of two keys in KEYS starting a COMMAND.
\nKEYS can be a string or a vector of two elements. Currently only elements
that corresponds to ascii codes in the range 32 to 126 can be used.
\nCOMMAND can be an interactive function, a string, or nil.
If COMMAND is nil, the key-combo is removed."
    ;;copy from key-chord-define
    (if (and(listp commands) (not (eq commands nil)))
	(let ((key keys))
	  (mapc '(lambda(command)
		   (key-combo-define keymap keys command)
		   (setq keys (concat keys key))
		   )commands))
      (let* ((key1 (substring keys 0 1))
	     (command (key-combo-lookup-key key1))
	     )
	(if (not (eq command 'key-combo))
	    (progn
	      ;;(message "ng")
	      (define-key keymap key1 'key-combo)
	      (define-key keymap
		(vector 'key-combo (intern key1)) command))
	  ;;(lookup-key )
	  )
	;;(message "%s" commands)
	(define-key keymap (vector 'key-combo (intern keys)) commands)
	))
    )

  (defun key-combo-define-global (keys command)
    "Define a key-combo of two keys in KEYS starting a COMMAND.
\nKEYS can be a string or a vector of two elements. Currently only elements
that corresponds to ascii codes in the range 32 to 126 can be used.
\nCOMMAND can be an interactive function, a string, or nil.
If COMMAND is nil, the key-combo is removed."
    ;;(interactive "sSet key chord globally (2 keys): \nCSet chord \"%s\" to command: ")
    (key-combo-define (current-global-map) keys command))
  )

(defun key-combo-define-local (keys command)
  "Define a key-combo of two keys in KEYS starting a COMMAND.
\nKEYS can be a string or a vector of two elements. Currently only elements
that corresponds to ascii codes in the range 32 to 126 can be used.
\nCOMMAND can be an interactive function, a string, or nil.
If COMMAND is nil, the key-combo is removed."
  ;;(interactive "sSet key chord locally (2 keys): \nCSet chord \"%s\" to command: ")
  (key-combo-define (current-local-map) keys command))

(defun key-combo-load-default ()
  (key-combo-load-default-1 (current-global-map))
  )

(defun key-combo-load-default-local ()
  (key-combo-load-default-1 (current-local-map))
  )

(defun key-combo-load-default-1 (map)
  (key-combo-define map (kbd "=") '(" = " " == " " === " "="))
  (key-combo-define map (kbd "+") '(" + " "++"))
	(key-combo-define map (kbd "&") '(" & " "&&"))
  ;;(key-combo-define map (kbd "-") '(" - " "-"))
  (key-combo-define map (kbd "=>") " => ")
  (key-combo-define map (kbd "=~") " =~ ")
  (key-combo-define map (kbd "(=") "(=`!!')")
  (key-combo-define map (kbd "<<") " << ")
  )

;;ok
;;(key-combo-define-global (kbd "=") '(" = " " == " "="))
;;(key-combo-define-global (kbd "(=") "(=`!!')")
;;ok
;;(key-combo-define-global (kbd "(") 'skeleton-pair-insert-maybe)
;;ok
;;(key-combo-define-global (kbd "=>") " => ")
;;(key-combo-define-global (kbd "=") '(" = " " == " "="))
;;ok
;;(key-combo-define-global (kbd "=") " = ")
;;ok
;;(global-set-key (kbd "=") 'key-combo)
;;ok
;;(key-combo-define-global (kbd "=") '(" = " " == " "="))
;;(key-combo-define-global (kbd "=>") " => ")
;;ng
;;(global-set-key (kbd "==") 'key-combo) => ng
;;ok
;;(key-combo-define-global (kbd "=>") " => ")
;;(key-combo-define-global (kbd "=") '(" = " " == " "="))
;;(key-combo-define-global (kbd ">") '(" > " " >> " ))
;;(key-combo-define-global (kbd "->") " -> ")
;;ok
;;(key-combo-define-global (kbd "=") " = ")
;;(key-combo-define-global (kbd "==") " == ")
;;(key-combo-define-global (kbd "===") "=")


;;skeleton(())後ろがかっこの場合囲む

;;clean up
;;(key-combo-define-global (kbd "=") '(nil nil nil))
;;(key-combo-define-global (kbd "-") '(nil nil))
;;(global-set-key(kbd "-") 'self-insert-command)

(dont-compile
	(when(fboundp 'expectations)
		(expectations
		 (desc "upcase")
		 (expect "FOO" (upcase "foo"))
		 (expect "BAR" (upcase "bAr"))
		 (expect "BAZ" (upcase "BAZ"))
		 )))

;;todo filter
;; filter for mode
;; filter for ""
;; unset key
;; support lamda func
(provide 'key-combo)
;;; key-combo.el ends here