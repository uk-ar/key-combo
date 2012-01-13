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
;; Version: 0.5
;; Keywords: keyboard input

;;; Commentary:

;; ########   Compatibility   ########################################
;;
;; Works with Emacs-23.2.1, 23.1.1

;; ########   Quick start   ########################################
;;
;; Add to your ~/.emacs
;;
;;  (require 'key-combo)
;;
;; and some chords, for example
;;
;;  (key-combo-define-global (kbd "=") '(" = " " == " " === " ))
;;  (key-combo-define-global (kbd "=>") " => ")
;;
;;
;; or load default settings
;;
;;  (key-combo-load-default)

;;; History:

;; Revision 0.5 2012/01/13 23:02:39
;; * Support function as key-combo command
;;
;; Revision 0.4
;; * Map key to minor mode to toggle enable and disable.
;;
;; Revision 0.3
;; * Not to cleanup when 1 sequence key
;; * Bugfix by tomykaira
;; * Refactoring
;; * Add test cases
;;
;; Revision 0.2
;; * First release
;;
;; Revision 0.1
;; * Initial revision

;; Code goes here
(require 'cl)


(defvar key-combo-loop-option 'only-same-key;'allways 'only-same-key 'never
  "Loop mode setting.
\n'allways:do loop both same key sequence and not same key sequence.
\n'only-same-key:do loop only same key sequence.
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

(defun key-combo-lookup (events)
  (let ((key
         (intern
          (if (characterp events)
              (char-to-string events)
            (mapconcat 'char-to-string events "")))))
    (key-combo-lookup-key (vector 'key-combo key))))

(defun key-combo-undo(command)
  (cond
   ((not (cdr-safe command)) nil);;no clean up
   ((commandp (cdr-safe command))
    (call-interactively (cdr command)))
   ((functionp (cdr-safe command))
    (funcall (cdr command)))
   (t (error "%s is not command" (cdr-safe command)))
   ))

(defun key-combo-command-execute(command)
  (cond
   ((not (listp command))
    (command-execute command))
   ((commandp (car command))
    (call-interactively (car command)))
   ((functionp (car command))
    (funcall (car command)))
   (t (error "%s is not command" (car command)))))

;;bug (C-/
(defun key-combo(arg)
  (interactive "P")
  ;;(call-interactively 'self-insert-command)
  (key-combo-command-execute
   '(self-insert-command . delete-backward-char))
  (undo-boundary)
  ;;for undo
  (let* ((same-key last-input-event)
         (all-command-keys (list last-input-event))
         (command (key-combo-lookup all-command-keys))
         (old-command '(self-insert-command . delete-backward-char)))
    (catch 'invalid-event
      (while command
        (key-combo-undo old-command)
        (key-combo-command-execute command)
        (undo-boundary);;for undo
        (if (not (characterp (read-event))) (throw 'invalid-event t))
        (setq same-key
              (cond ((eq key-combo-loop-option 'allways) t)
                    ((eq key-combo-loop-option 'only-same-key)
                     (if (eq last-input-event same-key) same-key nil))
                    ((eq key-combo-loop-option 'never) nil))
              old-command command)
        (setq all-command-keys (append all-command-keys
                                       (list last-input-event)))
        (setq command (key-combo-lookup all-command-keys))
        (if (and (not command) same-key);;for loop
            (progn
              (if (eq 2 (length all-command-keys)) (throw 'invalid-event t))
              (setq all-command-keys (char-to-string last-input-event))
              (setq command (key-combo-lookup all-command-keys))))
        );;end while
      );;end catch
    (setq unread-command-events
          (cons last-input-event unread-command-events))
    );;end let
  );;end key-combo

(defun key-combo-get-command(command)
  (unless (key-combo-elementp command)
    (error "%s is not command" command))
  (cond
   ((functionp command) command)
   ((listp command) command)
   ((not (stringp command)) nil)
   ((string-match "`!!'" command)
    (destructuring-bind (pre post) (split-string command "`!!'")
      (lexical-let ((pre pre) (post post))
        (cons
         (lambda()
           (insert pre)
           (save-excursion (insert post)))
         (lambda()
           (delete-backward-char (length pre))
           (delete-backward-char (- (length post)))))
        )))
   (t
    (lexical-let ((command command))
      (cons
       (lambda()
         (insert command))
       (lambda()
         (delete-backward-char (length command))))))
   );;end cond
  )

(defun key-combo-elementp (element)
  (or (or (functionp element)
          (stringp element)
          (null element))
      (and (or (functionp (car-safe element))
               (stringp (car-safe element)))
           (or (functionp (cdr-safe element))
                (stringp (cdr-safe element))
                (null (cdr-safe element))
                ))))

(defun key-combo-define (keymap keys commands)
  "Define in KEYMAP, a key-combo of two keys in KEYS starting a COMMAND.
\nKEYS can be a string or a vector of two elements. Currently only elements
that corresponds to ascii codes in the range 32 to 126 can be used.
\nCOMMAND can be an interactive function, a string, or nil.
If COMMAND is nil, the key-combo is removed."
  ;;copy from key-chord-define
  (cond
   ;;for sequence '(" = " " == ")
   ((not (key-combo-elementp commands))
    (let ((base-key keys)
          (seq-keys keys))
      (mapc '(lambda(command)
               (key-combo-define1 keymap seq-keys command)
               (setq seq-keys (concat seq-keys base-key)))
            commands)))
   (t
    (key-combo-define1 keymap keys commands))
   ))

;;(key-combo-define-global (kbd ">") '(">"))
(defun key-combo-define1 (keymap keys command)
  ;;copy from key-chord-define
  (unless (key-combo-elementp command)
    (error "%s is not command" command))
  (if (and (stringp (car-safe command));;define-key error for ("a")
           (null (cdr-safe command)))
      (setq command (car-safe command)))
  (let* ((key1 (substring keys 0 1))
         (command1 (key-combo-lookup-key key1)))
    (cond ((eq command nil)
           (define-key keymap key1 nil))
          ((not (eq command1 'key-combo))
           (define-key keymap key1 'key-combo))))
  (define-key keymap (vector 'key-combo (intern keys))
    (key-combo-get-command command)))

(defvar key-combo-mode-map (make-sparse-keymap))

(defun key-combo-define-global (keys command)
  "Define a key-combo of two keys in KEYS starting a COMMAND.
\nKEYS can be a string or a vector of two elements. Currently only elements
that corresponds to ascii codes in the range 32 to 126 can be used.
\nCOMMAND can be an interactive function, a string, or nil.
If COMMAND is nil, the key-combo is removed."
  ;;(interactive "sSet key chord globally (2 keys): \nCSet chord \"%s\" to command: ")
  (key-combo-define key-combo-mode-map keys command))

(defvar key-combo-default-alist
  '(("=" . (" = " " == " " === " ))
    ("=>" . " => ")
    (">" . (">"))
    (">=" . " >= ")))

(defun key-combo-unload-default ()
  (key-combo-load-default-1
   key-combo-mode-map
   (mapcar (lambda(x)
             (cons (car x)
                   (make-list (safe-length (cdr-safe x)) nil)))
           key-combo-default-alist)))

(defun key-combo-load-default ()
  (key-combo-load-default-1 key-combo-mode-map key-combo-default-alist)
  )
;;

(defun key-combo-load-default-1 (map keys)
 (dolist (key keys)
   (key-combo-define map (read-kbd-macro (car key))(cdr key)))
  ;; (key-combo-define map (kbd "=") '(" = " " == " " === " ))
  ;; ;; (key-combo-define map (kbd "+") '(" + " "++"))
  ;; ;; (key-combo-define map (kbd "&") '(" & " "&&"))
  ;; ;;(key-combo-define map (kbd "-") '(" - " "-"))
  ;; (key-combo-define map (kbd "=>") " => ")
  ;; (key-combo-define map (kbd ">") '(">"))
  ;; (key-combo-define map (kbd ">=") " >= ")
  ;; ;; (key-combo-define map (kbd "=~") " =~ ")
  ;; ;; (key-combo-define map (kbd "(=") "(=`!!')")
  ;; ;; (key-combo-define map (kbd "<<") " << ")
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

(defun test()
  (if (y-or-n-p "?")
      (split-window-horizontally 20)
    (split-window-vertically 10))
  1)
(dont-compile
  (when(fboundp 'expectations)
    (expectations
      (desc "key-combo")
      (expect ">>"
        (with-temp-buffer
          (setq unread-command-events (listify-key-sequence ">>\C-a"))
          (read-event)
          (setq last-command-event ?>)
          (call-interactively 'key-combo)
          (call-interactively 'key-combo)
          ;;(insert (char-to-string(car unread-command-events)))
          (buffer-string)
          ))
      (expect " = "
        (with-temp-buffer
          (setq unread-command-events (listify-key-sequence "=\C-a"))
          (read-key)
          (call-interactively 'key-combo)
          (buffer-string)
          ))
      (expect " = *"
        (with-temp-buffer
          (setq unread-command-events (listify-key-sequence "=*\C-a"))
          (read-event)
          (call-interactively 'key-combo)
          (insert (char-to-string(car unread-command-events)))
          (buffer-string)
          ))
      (expect " == "
        (with-temp-buffer
          (setq unread-command-events (listify-key-sequence "==\C-a"))
          (read-event)
          (call-interactively 'key-combo)
          (buffer-string)
          ))
      (expect " => "
        (with-temp-buffer
          (setq unread-command-events (listify-key-sequence "=>\C-a"))
          (read-event)
          (call-interactively 'key-combo)
          (buffer-string)
          ))
      (expect " === "
        (with-temp-buffer
          (setq unread-command-events (listify-key-sequence "===\C-a"))
          (read-event)
          (call-interactively 'key-combo)
          (buffer-string)
          ))
      ;;(key-combo-undo '(self-insert-command . delete-backword-char))
      ;;(key-combo-command-execute '(self-insert-command1 . delete-backward-char))
      ;;(desc "key-combo-undo")
      ;; (expect ""
      ;;   (with-temp-buffer
      ;;     (buffer-enable-undo)
      ;;     (key-combo-undo '((lambda() (insert "a")) . nil))
      ;;     (buffer-string)
      ;;     ))
      ;; (expect "a"
      ;;   (with-temp-buffer
      ;;     (buffer-enable-undo)
      ;;     (key-combo-undo '((lambda() (insert "a")) . (lambda() (insert "a"))))
      ;;     (buffer-string)
      ;;     ))
      ;; (desc "key-combo-command-execute")
      ;; (expect "a"
      ;;   (with-temp-buffer
      ;;     (buffer-enable-undo)
      ;;     (key-combo-undo '((lambda() (insert "a")) . (lambda() (insert "a"))))
      ;;     (buffer-string)
      ;;     ))
      (desc "key-combo-get-command")
      (expect "a"
        (with-temp-buffer
          (funcall (car (key-combo-get-command "a")))
          (buffer-string)
          ))
      (expect ""
        (with-temp-buffer
          (funcall (car (key-combo-get-command "a")))
          (funcall (cdr (key-combo-get-command "a")))
          (buffer-string)
          ))
      (expect t
        (with-temp-buffer
          (funcall (car (key-combo-get-command "a`!!'a")))
          (buffer-string)
          (and (equal (buffer-string) "aa") (eq (point) 2))
          ))
      (expect ""
        (with-temp-buffer
          (funcall (car (key-combo-get-command "a`!!'a")))
          (funcall (cdr (key-combo-get-command "a`!!'a")))
          (buffer-string)
          ))
      (desc "key-combo-define")
      (expect (error)
        (key-combo-define-global "a" 'wrong-command))
      (expect (no-error)
        (key-combo-define-global "a" 'self-insert-command))
      (expect (mock (define-key * * *) :times 2);;=> nil
        (stub key-combo-lookup-key => nil)
        (key-combo-define key-combo-mode-map "a" "a")
        )
      (expect (mock (define-key * * *) :times 1);;=> nil
        ;;(not-called define-key)
        (stub key-combo-lookup-key =>'key-combo)
        (key-combo-define key-combo-mode-map "a" "a")
        )
      (expect (mock (define-key * * *) :times 2);;(not-called define-key)
        ;;(mock   (define-key * * *) :times 0);;=> nil
        (stub key-combo-lookup-key =>'key-combo)
        (key-combo-define key-combo-mode-map "a" '("a" "bb"))
        )
      (desc "undo")
      (expect "="
        (with-temp-buffer
          (setq unread-command-events (listify-key-sequence "=\C-a"))
          (read-event)
          (buffer-enable-undo)
          (setq last-command-event ?=);;for self-insert-keys
          (call-interactively 'key-combo)
          (undo)
          (buffer-string)
          ))
      (expect " = "
        (with-temp-buffer
          (setq unread-command-events (listify-key-sequence "==\C-a"))
          (read-event)
          (buffer-enable-undo)
          (setq last-command-event ?=)
          (call-interactively 'key-combo)
          (undo)
          (buffer-string)
          ))
      (desc "loop")
      (expect " = "
        (with-temp-buffer
          (setq unread-command-events (listify-key-sequence "====\C-a"))
          (read-event)
          (call-interactively 'key-combo)
          (buffer-string)
          ))
      (expect " =>  = "
        (with-temp-buffer
          (setq unread-command-events (listify-key-sequence "=>=\C-a"))
          (read-event)
          (call-interactively 'key-combo)
          (read-event)
          (call-interactively 'key-combo)
          (buffer-string)
          ))
      (desc "key-combo-lookup-key")
      (expect " = "
        (with-temp-buffer
          (funcall
           (car (key-combo-lookup-key (vector 'key-combo (intern "=")))))
          (buffer-string)
          ))
      (expect " == "
        (with-temp-buffer
          (funcall
           (car (key-combo-lookup-key (vector 'key-combo (intern "==")))))
          (buffer-string)))
      (expect " => "
        (with-temp-buffer
          (funcall
           (car (key-combo-lookup-key (vector 'key-combo (intern "=>")))))
          (buffer-string)))
      (expect " === "
        (with-temp-buffer
          (funcall
           (car (key-combo-lookup-key (vector 'key-combo (intern "===")))))
          (buffer-string)))
      (expect nil
        (key-combo-lookup-key (vector 'key-combo (intern "===="))))
      (expect nil
        (key-combo-lookup-key (vector 'key-combo (intern "====="))))
      (desc "key-combo-lookup")
      (expect " = "
        (with-temp-buffer
          (funcall
           (car (key-combo-lookup [?=])))
          (buffer-string)))
      (expect " == "
        (with-temp-buffer
          (funcall
           (car (key-combo-lookup [?= ?=])))
          (buffer-string)))
      (expect " => "
        (with-temp-buffer
          (funcall
           (car (key-combo-lookup [?= ?>])))
          (buffer-string)))
      (expect " === "
        (with-temp-buffer
          (funcall
           (car (key-combo-lookup [?= ?= ?=])))
          (buffer-string)))
      (expect nil
        (key-combo-lookup [?= ?= ?= ?=]))
      (desc "key-combo-elementp")
      (expect t
        (every 'null
               ;;(identity
               (mapcar (lambda(command)
                         (progn (key-combo-define-global (kbd ">") command)
                                (null (key-combo-lookup ">"))))
                       '((">" . ">")
                         (">" . (lambda() ()))
                         ((lambda() ()) . ">")
                         (lambda()())
                         ((lambda() ()) . (lambda() ()))
                         ">"
                         ((lambda()()) . nil)
                         ((lambda()()))
                         (">" . nil)
                         (">")
                         (self-insert-command . delete-backward-char)
                         self-insert-command
                         (self-insert-command . nil)
                         (self-insert-command)
                         ))))
      (expect t
        (every 'identity
               (mapcar (lambda(x) (key-combo-elementp x))
                       '((">" . ">")
                         (">" . (lambda() ()))
                         ((lambda() ()) . ">")
                         (lambda()())
                         ((lambda() ()) . (lambda() ()))
                         ">"
                         nil
                         ((lambda()()) . nil)
                         ((lambda()()))
                         (">" . nil)
                         (">")
                         (self-insert-command . delete-backward-char)
                         self-insert-command
                         (self-insert-command . nil)
                         (self-insert-command)
                         ))))
      (expect t
        (every 'null
               (mapcar (lambda(x) (key-combo-elementp x))
                       '(((">" . ">"))
                         ((">" . (lambda() ())))
                         (((lambda()()) . ">"))
                         (((lambda()()) . (lambda() ())))
                         (nil)
                         ((self-insert-command . delete-backward-char))
                         ((nil . self-insert-command))
                         (wrong-command . wrong-command)
                         (wrong-command . nil)
                         (nil . wrong-command)
                         (nil . self-insert-command)
                         ))))
      (expect t
        (every 'null
               (mapcar (lambda(x) (key-combo-elementp x))
                       '((">" ">")
                         (">" (lambda()()))
                         ((lambda()()) ">")
                         ((lambda()()) ((lambda()())))
                         (((lambda()()) ">") ">")
                         ((">" (lambda()())) ">")
                         (">"                ((lambda()())">"))
                         (">"                (">" (lambda()())))
                         ((lambda()())     ((lambda()()) ">"))
                         (((lambda()()) ">") (lambda()()))
                         ((lambda()())     (">" (lambda()())))
                         ((">" (lambda()())) (lambda()()))
                         (">" self-insert-command)
                         (self-insert-command ">")
                         (self-insert-command (self-insert-command))
                         ((self-insert-command ">") ">")
                         ((">" self-insert-command) ">")
                         (">"                (self-insert-command">"))
                         (">"                (">" self-insert-command))
                         (self-insert-command     (self-insert-command ">"))
                         ((self-insert-command ">") self-insert-command)
                         (self-insert-command     (">" self-insert-command))
                         ((">" self-insert-command)  self-insert-command)))))
      ;; (desc "vertically")
      ;; (expect (mock (split-window-vertically 10))
      ;;         (stub y-or-n-p  => nil)
      ;;         (test))
      ;; (desc "horizontally")
      ;; (expect (mock (split-window-horizontally *))
      ;;         (stub y-or-n-p  => t)
      ;;         (test))
      ;; (desc "return")
      ;; (expect 1
      ;;         (stub y-or-n-p)
      ;;         (stub split-window-horizontally)
      ;;         (stub split-window-vertically)
      ;;         (test))
      )))

;;;###autoload
(define-minor-mode key-combo-mode
  "Toggle key combo."
  :global t
  :lighter " KC"
  :init-value t)

;;todo filter
;; filter for mode
;; filter for inside string ""
;; filter for inside comment ;;

;; support lamda func
(provide 'key-combo)
;;; key-combo.el ends here