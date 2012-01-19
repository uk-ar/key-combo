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
;; Version: 0.7
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

;; Revision 0.7 2012/01/17 21:25:10
;; * Insert white space dwim
;;
;; Revision 0.6 2012/01/16 21:17:01
;; * Allow cleanup function as nil
;; * Add key-combo-return function,
;; which can move to point of command beginning.
;; * Allow meta key for key-combo key.
;; * Save undo history when self-insert-command.
;;
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
(eval-when-compile (require 'cl))

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
  (let ((key (if (characterp events)
                 (single-key-description events)
               (key-description events))));;for vector
    (key-combo-lookup-key (vector 'key-combo (intern key)))))

(defun key-combo-lookup-original (events)
  (let ((key (if (characterp events)
                 (single-key-description events)
               (key-description events))));;for vector
    (prog2
        (key-combo-mode -1)
        (key-combo-lookup-key key)
      (key-combo-mode 1)
      )))

(defun key-combo-undo ()
  (let ((buffer-undo-list))
    (primitive-undo (1+ (key-combo-count-boundary key-combo-undo-list))
                    key-combo-undo-list)
    (setq key-combo-undo-list (append buffer-undo-list key-combo-undo-list))
    ))

(defun key-combo-command-execute (command)
  (let ((buffer-undo-list))
    (cond
     ((commandp command)
      (call-interactively command))
     ((functionp command)
      (funcall command))
     (t (error "%s is not command" (car command))))
    (undo-boundary)
    (if (boundp 'key-combo-undo-list)
        (setq key-combo-undo-list
              (append buffer-undo-list key-combo-undo-list)))
    )
  )

;;(browse-url "http://q.hatena.ne.jp/1226571494")
(defun key-combo-count-boundary (last-undo-list)
  (let ((count 0))
    (while (not (eq last-undo-list nil))
      (if (null (car last-undo-list)) (setq count (1+ count)))
      (setq last-undo-list (cdr last-undo-list)))
    count))

;;(key-combo-lookup-original ?=)
(defun key-combo (arg)
  (interactive "P")
  (let* ((same-key last-input-event)
         (all-command-keys (list last-input-event))
         (command (key-combo-lookup all-command-keys))
         (key-combo-undo-list))
    (unless (key-combo-lookup (list last-input-event))
      (error "invalid call"))
    (if (eq 'self-insert-command
            (key-combo-lookup-original last-input-event))
        (progn
          (key-combo-command-execute 'self-insert-command)
          (key-combo-undo)))
    (key-combo-set-start-position (cons (point) (window-start)))
    ;;for undo
    (catch 'invalid-event
      (while command
        (key-combo-undo)
        (key-combo-command-execute command)
        (if (not (characterp (read-event))) (throw 'invalid-event t))
        (setq same-key
              (cond ((eq key-combo-loop-option 'allways) t)
                    ((eq key-combo-loop-option 'only-same-key)
                     (if (eq last-input-event same-key) same-key nil))
                    ((eq key-combo-loop-option 'never) nil)))
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
    (setq buffer-undo-list (append key-combo-undo-list buffer-undo-list))
    );;end let
  );;end key-combo

(defun key-combo-smart-insert(string)
  (insert string)
  (if (eq ?  (aref string 0))
      (save-excursion
        (key-combo-return)
        (just-one-space)))
  )

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
         (lambda()
           (key-combo-smart-insert pre)
           (save-excursion (insert post))))
        ))
   (t
    (lexical-let ((command command))
       (lambda()
         (key-combo-smart-insert command)
       )))
   );;end cond
  )

(defun key-combo-elementp (element)
  (or (functionp element)
      (stringp element)
      (null element));;for unset
      )

(defun key-combo-define-seq (keymap keys commands)
  "Define in KEYMAP, a key-combo of two keys in KEYS starting a COMMAND.
\nKEYS can be a string or a vector of two elements. Currently only elements
that corresponds to ascii codes in the range 32 to 126 can be used.
\nCOMMAND can be an interactive function, a string, or nil.
If COMMAND is nil, the key-combo is removed."
  ;;copy from key-chord-define
  (cond
   ;;for sequence '(" = " " == ")
   ((and (not (key-combo-elementp commands))
         (consp (cdr-safe commands)))
    (let ((base-key keys)
          (seq-keys keys))
      (mapc '(lambda(command)
               (key-combo-define keymap seq-keys command)
               (setq seq-keys (concat seq-keys base-key)))
            commands)))
   (t
    (key-combo-define keymap keys commands))
   ))

(defun key-combo-define (keymap keys command)
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
  (define-key keymap (vector 'key-combo (intern (key-description keys)))
    (key-combo-get-command command)))

(defvar key-combo-mode-map (make-sparse-keymap))

(defun key-combo-define-global (keys command)
  "Define a key-combo of two keys in KEYS starting a COMMAND.
\nKEYS can be a string or a vector of two elements. Currently only elements
that corresponds to ascii codes in the range 32 to 126 can be used.
\nCOMMAND can be an interactive function, a string, or nil.
If COMMAND is nil, the key-combo is removed."
  ;;(interactive "sSet key chord globally (2 keys): \nCSet chord \"%s\" to command: ")
  (key-combo-define-seq key-combo-mode-map keys command))

(defvar key-combo-default-alist
  '(("=" . (" = " " == " " === " ))
    ("=>" . " => ")
    (">" . (">"))
    (">=" . " >= ")
    ("C-a" . ((back-to-indentation) (beginning-of-line) (lambda () (goto-char (point-min))) (key-combo-return)))
    ("C-e" . ((end-of-line) (lambda () (goto-char (point-max))) (key-combo-return)))
    ))

(defun key-combo-unload-default ()
  (interactive)
  (key-combo-load-default-1
   key-combo-mode-map
   (mapcar (lambda(x)
             (cons (car x)
                   (make-list (safe-length (cdr-safe x)) nil)))
           key-combo-default-alist)))

(defun key-combo-load-default ()
  (interactive)
  (key-combo-mode 1)
  (key-combo-load-default-1 key-combo-mode-map key-combo-default-alist)
  )

;;(declare-function key-combo-return "")
(lexical-let ((key-combo-start-position nil))
  (defun key-combo-set-start-position(pos)
    (setq key-combo-start-position pos))
  (defun key-combo-return ()
    "Return to the position when sequence of calls of the same command was started."
    (unless (eq key-combo-start-position nil)
      (progn
        (goto-char (car key-combo-start-position))
        (set-window-start (selected-window) (cdr key-combo-start-position)))))
  )

(defun key-combo-load-default-1 (map keys)
 (dolist (key keys)
   (key-combo-define-seq map (read-kbd-macro (car key))(cdr key)))
  ;; ;; (key-combo-define map (kbd "=~") " =~ ")
  ;; ;; (key-combo-define map (kbd "(=") "(=`!!')")
  ;; ;; (key-combo-define map (kbd "<<") " << ")
  )

(defun test()
  (if (y-or-n-p "?")
      (split-window-horizontally 20)
    (split-window-vertically 10))
  1)
(dont-compile
  (when(fboundp 'expectations)
    (expectations
      (desc "key-combo")
      ;; (expect ">"
      ;;   (with-temp-buffer
      ;;     (setq unread-command-events (listify-key-sequence ">>\C-a"))
      ;;     (read-event)
      ;;     (setq last-command-event ?>)
      ;;     (call-interactively 'key-combo)
      ;;     ;; (call-interactively 'key-combo)
      ;;     ;;(insert (char-to-string(car unread-command-events)))
      ;;     (buffer-string)
      ;;     ))
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
      (expect "I"
        (with-temp-buffer
          (insert "B\n IP")
          (setq unread-command-events (listify-key-sequence "\C-a\C-g"))
          (read-event)
          (call-interactively 'key-combo)
          (char-to-string(following-char))
          ))
      (expect " "
        (with-temp-buffer
          (insert "B\n IP")
          (setq unread-command-events (listify-key-sequence "\C-a\C-a\C-g"))
          (read-event)
          (call-interactively 'key-combo)
          (char-to-string(following-char))
          ))
      (expect "B"
        (with-temp-buffer
          (insert "B\n IP")
          (setq unread-command-events (listify-key-sequence "\C-a\C-a\C-a\C-g"))
          (read-event)
          (call-interactively 'key-combo)
          (char-to-string(following-char))
          ))
      (expect "P"
        (with-temp-buffer
          (insert "B\n IP")
          (setq unread-command-events
                (listify-key-sequence "\C-a\C-a\C-a\C-a\C-g"))
          (read-event)
          (backward-char)
          (call-interactively 'key-combo)
          (char-to-string(following-char))
          ))
      (desc "key-combo-undo")
      ;; (expect ""
      ;;   (with-temp-buffer
      ;;     (buffer-enable-undo)
      ;;     (key-combo-undo '((lambda() (insert "a")) . nil))
      ;;     (buffer-string)
      ;;     ))
      ;; (expect "a"
      ;;   (with-temp-buffer
      ;;     (buffer-enable-undo)
      ;;     (key-combo-undo
      ;;      '((lambda() (insert "a")) . (lambda() (insert "a"))))
      ;;     (buffer-string)
      ;;     ))
      ;; (expect "b"
      ;;   (with-temp-buffer
      ;;     (buffer-enable-undo)
      ;;     (let ((last-command-event ?b))
      ;;       (key-combo-undo
      ;;        '((lambda() (insert "a")) . self-insert-command)))
      ;;     (buffer-string)
      ;;     ))
      ;; (expect (error)
      ;;   (with-temp-buffer
      ;;     (buffer-enable-undo)
      ;;     (key-combo-undo '((lambda() (insert "a")) . wrong-command))
      ;;     (buffer-string)
      ;;     ))
      (desc "key-combo-command-execute")
      (expect "a"
        (with-temp-buffer
          (buffer-enable-undo)
          (key-combo-command-execute (lambda() (insert "a")))
          (buffer-string)
          ))
      (expect (error)
        (with-temp-buffer
          (buffer-enable-undo)
          (key-combo-command-execute 'wrong-command)
          (buffer-string)
          ))
      (expect "b"
        (with-temp-buffer
          (buffer-enable-undo)
          (let ((last-command-event ?b))
            (key-combo-command-execute 'self-insert-command))
          (buffer-string)
          ))
      (desc "key-combo-get-command")
      (expect "a"
        (with-temp-buffer
          (funcall (key-combo-get-command "a"))
          (buffer-string)
          ))
      ;; (expect ""
      ;;   (with-temp-buffer
      ;;     (buffer-enable-undo)
      ;;     (funcall (key-combo-get-command "a"))
      ;;     (key-combo-undo)
      ;;     ;;(funcall (cdr (key-combo-get-command "a")))
      ;;     (buffer-string)
      ;;     ))
      (expect t
        (with-temp-buffer
          (funcall (key-combo-get-command "a`!!'a"))
          (buffer-string)
          (and (equal (buffer-string) "aa") (eq (point) 2))
          ))
      ;; (expect ""
      ;;   (with-temp-buffer
      ;;     (buffer-enable-undo)
      ;;     (funcall (key-combo-get-command "a`!!'a"))
      ;;     (key-combo-undo)
      ;;     ;;(funcall (key-combo-get-command "a`!!'a"))
      ;;     (buffer-string)
      ;;     ))
      (desc "key-combo-define")
      (expect (error)
        (key-combo-define-global "a" 'wrong-command))
      (expect (no-error)
        (key-combo-define-global "a" 'self-insert-command))
      (expect (no-error)
        (key-combo-define-global (kbd "C-M-g") 'self-insert-command))
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
        (key-combo-define-seq key-combo-mode-map "a" '("a" "bb"))
        )
      (desc "undo")
      (expect "="
        (with-temp-buffer
          (setq unread-command-events (listify-key-sequence "=\C-a"))
          (read-event)
          (buffer-enable-undo)
          (setq last-command-event ?=);;for self-insert-command
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
      (expect " => = "
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
           (key-combo-lookup-key
                 (vector 'key-combo (intern (key-description "=")))))
          (buffer-string)
          ))
      (expect " == "
        (with-temp-buffer
          (funcall
           (key-combo-lookup-key
                 (vector 'key-combo (intern (key-description "==")))))
          (buffer-string)))
      (expect " => "
        (with-temp-buffer
          (funcall
           (key-combo-lookup-key
                 (vector 'key-combo (intern (key-description "=>")))))
          (buffer-string)))
      (expect " === "
        (with-temp-buffer
          (funcall
           (key-combo-lookup-key
                 (vector 'key-combo (intern (key-description "===")))))
          (buffer-string)))
      (expect nil
        (key-combo-lookup-key
         (vector 'key-combo (intern (key-description "====")))))
      (expect nil
        (key-combo-lookup-key
         (vector 'key-combo (intern (key-description "=====")))))
      (expect 'self-insert-command
        (prog2
            (key-combo-mode 0)
            (key-combo-lookup-key (kbd "="))
          (key-combo-mode 1)))
      (desc "key-combo-lookup")
      (expect " = "
        (with-temp-buffer
          (funcall
           (key-combo-lookup "="))
          (buffer-string)))
      (expect " == "
        (with-temp-buffer
          (funcall
           (key-combo-lookup "=="))
          (buffer-string)))
      (expect " == "
        (with-temp-buffer
          (key-combo-define-global (kbd "C-M-h") " == ")
          (funcall
           (key-combo-lookup (kbd "C-M-h")))
          (buffer-string)))
      (expect " === "
        (with-temp-buffer
          (key-combo-define-global (kbd "C-M-h C-M-h") " === ")
          (funcall
           (key-combo-lookup (kbd "C-M-h C-M-h")))
          (buffer-string)))
      (expect " = "
        (with-temp-buffer
          (funcall
           (key-combo-lookup [?=]))
          (buffer-string)))
      (expect " == "
        (with-temp-buffer
          (funcall
           (key-combo-lookup [?= ?=]))
          (buffer-string)))
      (expect " => "
        (with-temp-buffer
          (funcall
           (key-combo-lookup [?= ?>]))
          (buffer-string)))
      (expect " === "
        (with-temp-buffer
          (funcall
           (key-combo-lookup [?= ?= ?=]))
          (buffer-string)))
      (expect nil
        (key-combo-lookup [?= ?= ?= ?=]))
      (desc "key-combo-lookup-original")
      (expect 'self-insert-command
        (key-combo-lookup-original (key-description (list ?b))))
      (expect 'self-insert-command
        (key-combo-lookup-original "b"))
      (expect 'self-insert-command
        (key-combo-lookup-original (key-description (list ?=))))
      (expect 'self-insert-command
        (key-combo-lookup-original "b"))
      (desc "key-combo-elementp")
      (expect t
        (every 'null
        ;;(identity
               (mapcar (lambda(command)
                         (progn (key-combo-define-global ">>" command)
                                (null (key-combo-lookup ">>"))))
                       '((lambda()())
                         ">"
                         self-insert-command
                         ))))
      (expect t
        (every 'identity
               (mapcar (lambda(x) (key-combo-elementp x))
                       '((lambda()())
                         ">"
                         nil
                         self-insert-command
                         ))))
      (expect t
        (every 'null
               (mapcar (lambda(x) (key-combo-elementp x))
                       '(((">"))
                         (((lambda()())))
                         (nil)
                         ((self-insert-command))
                         (wrong-command)
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
  :lighter " KC")

;;todo filter
;; filter for mode
;; filter for inside string ""
;; filter for inside comment ;;

;; support lamda func
(provide 'key-combo)
;;; key-combo.el ends here