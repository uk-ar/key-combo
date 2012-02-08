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
;; Version: 1.0
;; Keywords: keyboard input

;;; Commentary:

;; ########   Compatibility   ########################################
;;
;; Works with Emacs-23.2.1, 23.1.1

;; ########   Quick start   ########################################
;;
;; Add to your ~/.emacs
;;
;; (require 'key-combo)
;; (key-combo-mode 1)
;;
;; and some chords, for example
;;
;;  (key-combo-define-global (kbd "=") '(" = " " == " " === " ))
;;  (key-combo-define-global (kbd "=>") " => ")
;;
;; or load default settings
;;
;;  (key-combo-load-default)

;;; History:

;; Revision 1.0 2012/01/31 22:03:50
;; * Change clean-up function to use undo
;;
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
(require 'cl)
;; for remove-if

(defvar key-combo-loop-option 'only-same-key;'allways 'only-same-key 'never
  "Loop mode setting.
\n'allways:do loop both same key sequence and not same key sequence.
\n'only-same-key:do loop only same key sequence.
\n'never:don't loop.")

(defun key-combo-describe ()
  "List key combo bindings in a help buffer."
  (interactive)
  (describe-bindings [key-combo]))

(defun key-combo-lookup (events)
  (let ((key ;;string
         (if (or (vectorp events) (consp events))
             (key-description events);;for vector
           (single-key-description events))))
    (key-binding (vector 'key-combo (intern key)))))

(defun key-combo-execute-orignal ()
  (interactive)
  (call-interactively (key-binding (vector last-input-event)))
  )

(defun key-combo-undo ()
  (if (boundp 'key-combo-undo-list)
      (let ((buffer-undo-list))
        (primitive-undo (1+ (key-combo-count-boundary key-combo-undo-list))
                        key-combo-undo-list)
        (setq key-combo-undo-list
              (append buffer-undo-list key-combo-undo-list)))))

(defun key-combo-command-execute (command)
  (let ((buffer-undo-list))
    (cond
     ((commandp command)
      (call-interactively command))
     (t (funcall command)))
    (undo-boundary)
    (if (boundp 'key-combo-undo-list)
        (setq key-combo-undo-list
              (append buffer-undo-list key-combo-undo-list)))))

(defun key-combo-comment-or-stringp (&optional pos)
  (setq pos (or pos (point)))
  (memq (get-text-property pos 'face)
        '(font-lock-comment-face font-lock-doc-face
                                 font-lock-string-face))
  )

;;(browse-url "http://q.hatena.ne.jp/1226571494")
(defun key-combo-count-boundary (last-undo-list)
  (length (remove-if-not 'null last-undo-list)))

(defun* key-combo (arg)
  (interactive "P")
  (let* ((same-key last-input-event)
         (all-command-keys (list last-input-event))
         (command (key-combo-lookup all-command-keys))
         (key-combo-undo-list))
    (if (eq 'self-insert-command
            (key-binding (vector last-input-event)))
        (progn
          (key-combo-command-execute 'self-insert-command)
          (if (key-combo-comment-or-stringp) (return-from key-combo nil))
          ;; undo in first loop
          ))
    (key-combo-set-start-position (cons (point) (window-start)))
    ;;for undo
    (catch 'invalid-event
      (while command
        (key-combo-undo)
        (key-combo-command-execute command)
        (read-event)
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
              (setq all-command-keys (list last-input-event))
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
      (null element));;for unset key
  )

(defun key-combo-define (keymap keys commands)
  "Define in KEYMAP, a key-combo of two keys in KEYS starting a COMMAND.
\nKEYS can be a string or a vector of two elements. Currently only elements
that corresponds to ascii codes in the range 32 to 126 can be used.
\nCOMMAND can be an interactive function, a string, or nil.
If COMMAND is nil, the key-combo is removed."
  ;;copy from key-chord-define
  (cond
   ;;for sequence '(" = " " == ")
   ((and (not (key-combo-elementp commands))
         (key-combo-elementp (car-safe commands)))
    (let* ((base-key (listify-key-sequence keys))
           (seq-keys base-key));;list
      (mapc '(lambda(command)
               (key-combo-define keymap (vconcat seq-keys) command)
               (setq seq-keys
                     (append seq-keys base-key)))
            commands)))
   (t
    (unless (key-combo-elementp commands)
      (error "%s is not command" commands))
    (define-key keymap
      (vector 'key-combo (intern (key-description keys)))
      (key-combo-get-command commands))
    )
   ))

(defun key-combo-define-global (keys command)
  "Define a key-combo of two keys in KEYS starting a COMMAND.
\nKEYS can be a string or a vector of two elements. Currently only elements
that corresponds to ascii codes in the range 32 to 126 can be used.
\nCOMMAND can be an interactive function, a string, or nil.
If COMMAND is nil, the key-combo is removed."
  ;;(interactive "sSet key chord globally (2 keys): \nCSet chord \"%s\" to command: ")
  (key-combo-define (current-global-map) keys command))

(defun key-combo-define-local (keys command)
  "Define a key-combo of two keys in KEYS starting a COMMAND.
\nKEYS can be a string or a vector of two elements. Currently only elements
that corresponds to ascii codes in the range 32 to 126 can be used.
\nCOMMAND can be an interactive function, a string, or nil.
If COMMAND is nil, the key-combo is removed."
  ;;(interactive "sSet key chord globally (2 keys): \nCSet chord \"%s\" to command: ")
  (key-combo-define (current-local-map) keys command))

;; < { [ should map bracket command
(defvar key-combo-global-default
  '(("="  . (" = " " == " " === " ));;" === " for js
    ("=>" . " => ")
    (","  . ", ")
    ;; use beginning-of-buffer for keydescription
    ;; (lambda () (goto-char (point-min)))
    ("C-a"   . (back-to-indentation beginning-of-line
                                  beginning-of-buffer key-combo-return))
    ("C-e"   . (end-of-line end-of-buffer key-combo-return))
    ("C-M-x" . (key-combo-execute-orignal
                (lambda ()
                  (let ((current-prefix-arg '(4)))
                    (call-interactively 'eval-defun)))))
    ))

(defvar key-combo-lisp-default
  '(("."  . " . ")
    (";"  . (";; " ";;; " "; "))
    ("="  . "= ")
    (">=" . ">= ")
    ;; ("-" . self-insert-command)
    ;; ("/" . ("/`!!'/" "/* `!!' */") );;for regexp, comment
    ))

(defvar key-combo-lisp-mode-hooks
  '(lisp-mode-hook
    emacs-lisp-mode-hook
    lisp-interaction-mode-hook
    inferior-gauche-mode-hook
    scheme-mode-hook))

(defmacro define-key-combo-load (name)
  `(defun ,(intern (concat "key-combo-load-" name "-default")) ()
     (dolist (key ,(intern (concat "key-combo-" name "-default")))
       (key-combo-define-local (read-kbd-macro (car key)) (cdr key)))
     ))

(define-key-combo-load "lisp")

(defvar key-combo-c-mode-hooks
  '(c-mode-common-hook
    c++-mode-hook
    php-mode-hook
    ruby-mode-hook
    cperl-mode-hook
    javascript-mode-hook
    js-mode-hook
    js2-mode-hook))

(defvar key-combo-c-default
  '(("+"  . (" + " "++"))
    ("+=" . " += ")
    ("-"  . (" - " "--"))
    ("-=" . " -= ")
    (">"  . (" > " " >> "))
    (">=" . " >= ")
    ("=~" . " =~ ");;for ruby regexp
    ("%"  . " % ")
    ("^"  . " ^ ");; c XOR
    ("!" . key-combo-execute-orignal) ;;not " !" because of ruby symbol
    ("!=" . " != ")
    ;; (":" . " :");;only for ruby
    ;; ("&"  . (" & " " && ")) ;;not use because c pointer
    ;; ("*"  . " * " ) ;;not use because c pointer
    ("?" . "? ");; for ternary operator
    ;; ("<"  . (" < " " << "));; not use because of c include
    ("<" . (key-combo-execute-orignal " << "))
    ("<=" . " <= ")
    ;; ("|"  . (" | " " || ")) ;;ruby block
    ;; ("/" . (" / " "// " "/`!!'/")) ;; devision,comment start or regexp
    ("/" . key-combo-execute-orignal)
    ("/*" . "/* `!!' */")
    ))

(define-key-combo-load "c")

(defvar key-combo-objc-default
  (append '(("@"  . "@\"`!!'\""))
          key-combo-c-default
          ))

(define-key-combo-load "objc")

(defvar key-combo-html-mode-hooks
  '(html-mode-hook
    css-mode-hook))

(defvar key-combo-html-default
  '((":"  . ": ")
    ))

(define-key-combo-load "html")

(defvar key-combo-org-default
  '(("C-a" . (org-beginning-of-line
             beginning-of-buffer
             key-combo-return));;back-to-indentation
    ("C-e" . (org-end-of-line
              end-of-buffer
              key-combo-return))
    ))

(define-key-combo-load "org")

(defun key-combo-load-default ()
  (interactive)
  (key-combo-mode 1)
  (key-combo-load-default-1 (current-global-map)
                            key-combo-global-default)
  (key-combo-load-by-hooks key-combo-lisp-mode-hooks
                           'key-combo-load-lisp-default)
  (key-combo-load-by-hooks key-combo-c-mode-hooks
                           'key-combo-load-c-default)
  (key-combo-load-by-hooks '(objc-mode-hook)
                           'key-combo-load-objc-default)
  (key-combo-load-by-hooks key-combo-html-mode-hooks
                           'key-combo-load-objc-default)
  (key-combo-load-by-hooks '(org-mode-hook)
                           'key-combo-load-org-default)
  )

(defun key-combo-load-by-hooks (hooks func)
  (dolist (hook hooks)
      (add-hook hook func)))

(defun key-combo-load-default-1 (map keys)
  (dolist (key keys)
    (key-combo-define map (read-kbd-macro (car key)) (cdr key))))

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

(defun test1()
  (interactive)
  (message "test1")
  )
(defun test2()
  (interactive)
  (message "test2")
  )
(dont-compile
  (when(fboundp 'expectations)
    (expectations
      (desc "key-combo")
      ;; add i-search mode
      (expect nil
        (with-temp-buffer
          (key-combo-mode -1)
          (if (memq 'key-combo-pre-command-function pre-command-hook) t nil)
          ))
      (expect t
        (with-temp-buffer
          (key-combo-mode 1)
          (if (memq 'key-combo-pre-command-function pre-command-hook) t nil)
          ))
      (expect ">"
        (with-temp-buffer
          (setq unread-command-events (listify-key-sequence ">\C-a"))
          (read-event)
          (setq last-command-event ?>)
          (call-interactively 'key-combo)
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
      ;;(mock (edebug-defun) :times 1);;=> nil
      ;;(mock (expectations-eval-defun) :times 1);;=> nil
      (expect (mock (test2 *) :times 1);;=> nil
        (with-temp-buffer
          (key-combo-define-global (kbd "C-M-d") '(test1 test2))
          (setq unread-command-events
                (listify-key-sequence "\C-\M-d\C-\M-d\C-g"));;\C-\M-x
          (read-event)
          (call-interactively 'key-combo)
          ))
      (expect (mock (test2 *) :times 1);;=> nil
        (with-temp-buffer
          (key-combo-define-global (kbd "C-M-d") '(test1 test2))
          (setq unread-command-events
                (listify-key-sequence "\C-\M-d\C-\M-d\C-\M-g\C-g"));;\C-\M-x
          (read-event)
          (call-interactively 'key-combo)
          ))
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
      (expect t
        (with-temp-buffer
          (funcall (key-combo-get-command "a`!!'a"))
          (buffer-string)
          (and (equal (buffer-string) "aa") (eq (point) 2))
          ))
      (desc "key-combo-undo")
      (expect ""
        (with-temp-buffer
          (buffer-enable-undo)
          (let((key-combo-undo-list))
            (key-combo-command-execute (lambda() (insert "a")))
            (key-combo-undo)
            )
          (buffer-string)
          ))
      (expect ""
        (with-temp-buffer
          (buffer-enable-undo)
          (let((key-combo-undo-list))
            (key-combo-command-execute (key-combo-get-command "a`!!'a"))
            (key-combo-undo)
            )
          (buffer-string)
          ))
      (desc "key-combo-define")
      (expect (error)
        (key-combo-define-global "a" 'wrong-command))
      (expect (no-error)
        (key-combo-define-global "a" 'self-insert-command))
      (expect (no-error)
        (key-combo-define-global "a" nil))
      (expect (no-error)
        (key-combo-define-global (kbd "C-M-g") 'self-insert-command))
      (expect (mock (define-key * * *) :times 1);;=> nil
        (stub key-binding => nil)
        (key-combo-define-local "a" "a")
        )
      (expect (mock (define-key * * *) :times 1);;=> nil
        ;;(not-called define-key)
        (stub key-binding =>'key-combo)
        (key-combo-define-local "a" '("a"))
        )
      (expect (mock (define-key * * *) :times 2);;(not-called define-key)
        ;;(mock   (define-key * * *) :times 0);;=> nil
        (stub key-binding =>'key-combo)
        (key-combo-define-local "a" '("a" "bb"))
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
      (desc "key-combo-lookup")
      ;;(key-combo-lookup "=");;ok
      ;;(key-combo-lookup "= =");;ok
      ;;(key-combo-lookup "==");;ng

      ;;(key-combo-lookup 134217752)
      ;;(single-key-description 134217752)
      ;;(key-combo-lookup-original 134217752)
      ;;(key-combo-lookup '(134217752 134217752))
      ;;(key-combo-lookup '(134217752))

      ;;(key-combo-lookup '("="))
      ;;(key-combo-load-default)
      ;; (listify-key-sequence "a")
      ;; (listify-key-sequence (kbd "C-M-x"))
      ;;read-kbd-macro
      ;; (vconcat (append '(134217752) '(134217752)))
      ;; (append '(97) '(97))
      ;;(key-description '[134217752 134217752])
      ;;(key-description '[97 97])
      ;;(substring '[134217752 134217752] 0 1)

      ;;(key-combo-lookup-original 134217752)
      ;;(key-combo-lookup-original "C-M-x")

      ;;(single-key-description "C-M-x")
      ;;(single-key-description "a")
      ;;(single-key-description "C-M-x")
      ;;(key-binding "C-M-x")
      ;;(key-binding [134217752]);;ok
      ;;(key-binding '(134217752));;ng
      ;;(key-binding 134217752);;ng
      ;;(key-combo-load-default)
      (expect " = "
        (with-temp-buffer
          (funcall
           (key-combo-lookup "="))
          (buffer-string)))
      (expect " == "
        (with-temp-buffer
          (funcall
           (key-combo-lookup "= ="))
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
          (setq unread-command-events
                (listify-key-sequence "\C-\M-h\C-\M-h\C-a"))
          (read-event)
          (call-interactively 'key-combo)
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
      (desc "key-combo-elementp")
      (expect t
        (every 'identity
               ;;(identity
               (mapcar (lambda(command)
                         (progn (key-combo-define-global ">>" command)
                                (identity (key-combo-lookup "> >"))))
                       '((lambda()())
                         ">"
                         ;;nil
                         self-insert-command
                         ((lambda()()))
                         (">")
                         ;;(nil)
                         (self-insert-command)
                         (">" ">")
                         (">" (lambda()()))
                         ((lambda()()) ">")
                         ((lambda()()) (lambda()()))
                         (">" self-insert-command)
                         (self-insert-command ">")
                         (self-insert-command self-insert-command)
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
                       '((">")
                         ((lambda()()))
                         (nil)
                         (self-insert-command)
                         wrong-command
                         ))))
      )))

(defvar key-combo-disable-faces
  )

(defun key-combo-pre-command-function ()
  (if (and
       key-combo-mode
       (not (minibufferp))
       (not isearch-mode)
       (key-combo-lookup (this-command-keys-vector)))
      ;;(progn (message "pre")
      (setq this-command 'key-combo)
    ;;)
    ))

;;;###autoload
(define-minor-mode key-combo-mode
  "Toggle key combo."
  :global t
  :lighter " KC"
  :group 'key-combo
  (if key-combo-mode
      (add-hook 'pre-command-hook
                ;;post-self-insert-hook
                #'key-combo-pre-command-function)
    (remove-hook 'pre-command-hook
                 #'key-combo-pre-command-function))
  )

;;todo filter
;; filter for mode
;; filter for inside string ""
;; filter for inside comment ;;

;; support lamda func
(provide 'key-combo)
;;; key-combo.el ends here
