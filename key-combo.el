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
;; Version: 0.2
;; Keywords: keyboard input

;;; Commentary:

;; ########   Compatibility   ########################################
;;
;; Works with Emacs-23.2.1

;; ########   Quick start   ########################################
;;
;; Add to your ~/.emacs
;;
;; (require 'key-combo)
;;
;; and some chords, for example
;;
;;      (key-chord-define-global "hj"     'undo)
;;      (key-chord-define-global ",."     "<>\C-b")

;; (require 'key-combo)
;; (key-combo-load-default)

;; Code goes here
(require 'cl)
(progn

  (defvar key-combo-loop-option 'only-samekey;'allways 'only-samekey 'never
    "Loop mode setting.
\n'allways:do loop both same key sequence and not same key sequence.
\n'only-samekey:do loop only same key sequence.
\n'never:don't loop.")

  (defun key-combo-lookup-key1 (keymap key)
    ;; copy from key-chord-lookup-key
    "Like lookup-key but no third arg and no numeric return value."
    (let ((res (lookup-key keymap key)))
      (if (numberp res)
          nil
        ;; else
        res)))

  (defun key-combo-describe ()
    "List key combo bindings in a help buffer."
    (interactive)
    (describe-bindings [key-combo]))

  (defun key-combo-lookup-key (key)
    ;; copy from key-chord-lookup-key
    "Lookup KEY in all current key maps."
    (let ((maps (current-minor-mode-maps))
     res)
      (while (and maps (not res))
        (setq res (key-combo-lookup-key1 (car maps) key)
              maps (cdr maps)))
      (or res
          (if (current-local-map)
              (key-combo-lookup-key1 (current-local-map) key))
          (key-combo-lookup-key1 (current-global-map) key))))

  (defun key-combo-undo(command)
    (cond ((and command (stringp command))
           (if (string-match "`!!'" command)
               (progn
                 (destructuring-bind (pre post)
                     (split-string command "`!!'")
                   (delete-backward-char (length pre))
                   (delete-backward-char (- (length post)))
                   ))
             (delete-backward-char (length command))))
          (t (flet ((message (format &rest args) (identity args)))
               (undo))              ;execute undo whithout message.
             (undo-boundary))));;

  (defun key-combo-get-command(same-key all-command-keys)
    (let ((new-command-keys nil))
      (setq command
            (or
             (progn
               (setq new-command-keys all-command-keys)
               (key-combo-lookup-key
                (vector 'key-combo
                        (intern all-command-keys))))
             (if (and same-key (characterp last-input-event))
                 (progn       ;for loop  = == === =
                   (setq new-command-keys (char-to-string last-input-event))
                   (key-combo-lookup-key
                    (vector 'key-combo
                            (intern (char-to-string last-input-event)))))
               nil
               )))
      (list command new-command-keys)
      ))

  ;;bug (C-/
  (defun key-combo()
    (interactive)
    ;;(message "in")
    (let ((first-char (this-command-keys)))
      (insert first-char)
      (undo-boundary)
      (delete-backward-char (length first-char))
      )
    ;;for undo
    (let* ((next-char nil)
           (same-key t)
           (first-char last-input-event)
           (all-command-keys (format "%c" first-char))
           (command (car (key-combo-get-command same-key all-command-keys)))
           (old-command nil)
          )
      (while command
        (message "*start loop")
        (message "*%s" all-command-keys)
        (if next-char
            (progn
              ;;(message "normal undo")
              (key-combo-undo old-command)))
        (cond ((and command (stringp command))
               (if (string-match "`!!'" command)
                   (progn
                     (destructuring-bind (pre post)
                         (split-string command "`!!'")
                       (insert pre)
                       (save-excursion (insert post))))
                 (insert command)))
              (t (command-execute command)))
        (undo-boundary);;for undo
        (setq same-key
              (cond ((eq key-combo-loop-option 'allways) t)
                    ((eq key-combo-loop-option 'only-samekey)
                     (and same-key (eq last-input-event first-char)))
                    ((eq key-combo-loop-option 'never) nil))
              next-char (read-event)
              old-command command
              all-command-keys (if (characterp next-char)
                                   (format "%c%s" next-char all-command-keys)
                                 "")
              )
        (destructuring-bind (comm all)
            (key-combo-get-command same-key all-command-keys)
          (setq command comm
                all-command-keys all)
            )
        ;;(message "s:%s n:%c o:%s" same-key next-char old-command)
        ;;(setq debug-on-error t)
        ;; (message "this-keys3:%s" (this-command-keys))
        ;; (message "keys3:%s" command)

        ;; (message "n3:%s" ;;(key-combo-lookup-key
        ;;          (if next-char
        ;;              (vector 'key-combo
        ;;                      ( intern(char-to-string next-char))
        ;;                      )))
        ;; (message "lc:%s lce:%c tck:%s lcc:%c lie:%c lef:%s"
        ;;          last-command last-command-event
        ;;          (this-command-keys)
        ;;          last-command-char
        ;;          last-input-event
        ;;          last-event-frame
        ;;          );;(message "l2:%c" last-input-event)
        );;end while
      (setq unread-command-events
            (cons next-char unread-command-events))
      );;end let
    );;end key-combo

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
                   (setq keys (concat keys key)))
                commands))
      (let* ((key1 (substring keys 0 1))
             (command (key-combo-lookup-key key1)))
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
;;
;;(key-combo-load-default)
(defun key-combo-load-default-local ()
  (key-combo-load-default-1 (current-local-map))
  )

(defun key-combo-load-default-1 (map)
  (key-combo-define map (kbd "=") '(" = " " == " " === " ))
  ;; (key-combo-define map (kbd "+") '(" + " "++"))
  ;; (key-combo-define map (kbd "&") '(" & " "&&"))
  ;;(key-combo-define map (kbd "-") '(" - " "-"))
  (key-combo-define map (kbd "=>") " => ")
  (key-combo-define map (kbd ">") '(">"))
  (key-combo-define map (kbd ">=") " >= ")
  ;; (key-combo-define map (kbd "=~") " =~ ")
  ;; (key-combo-define map (kbd "(=") "(=`!!')")
  ;; (key-combo-define map (kbd "<<") " << ")
  )
;;(key-combo-load-default)
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

(defun test()
 (if (y-or-n-p "?")
     (split-window-horizontally 20)
   (split-window-vertically 10))
 1)
(dont-compile
  (when(fboundp 'expectations)
    (expectations
     (desc "upcase")
     (expect "FOO" (upcase "foo"))
     (expect "BAR" (upcase "bAr"))
     (expect "BAZ" (upcase "BAZ"))
     (desc "vertically")
     (expect (mock (split-window-vertically 10))
             (stub y-or-n-p  => nil)
             (test))
     (desc "horizontally")
     (expect (mock (split-window-horizontally *))
             (stub y-or-n-p  => t)
             (test))
     (desc "return")
     (expect 1
             (stub y-or-n-p)
             (stub split-window-horizontally)
             (stub split-window-vertically)
             (test))
     )))

(key-combo-lookup-key (vector 'key-combo (intern "="))) ; => " = "
(key-combo-lookup-key (vector 'key-combo (intern "=="))) ; => " == "
(key-combo-lookup-key (vector 'key-combo (intern "===")))  ; => " === "
(key-combo-lookup-key (vector 'key-combo (intern "===="))) ; => nil
(key-combo-lookup-key (vector 'key-combo (intern "====="))) ; => nil

(key-combo-get-command t "=")         ; => (" = " "=")
(key-combo-get-command t "==")        ; => (" == " "==")
(let((last-input-event ?=))
  (key-combo-get-command t "==="))    ; => (" === " "===")
(let((last-input-event ?=))
  (key-combo-get-command t "===="))   ; => (" = " "====")
(let((last-input-event ?=))
  (key-combo-get-command t "====="))  ; => (" = " "=====")
(let((last-input-event ?*))
  (key-combo-get-command t "===*"))   ; => (nil "===*")

;;todo filter
;; filter for mode
;; filter for ""
;; unset key
;; support lamda func
(provide 'key-combo)
;;; key-combo.el ends here