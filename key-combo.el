;;; key-combo.el --- map key sequence to commands

;;-------------------------------------------------------------------
;;
;; Copyright (C) 2011, 2012 Yuuki Arisawa
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
;; URL: https://github.com/uk-ar/key-combo
;; Created: 30 November 2011
;; Version: 1.5.1
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

;; Revision 1.5.1 2012/06/06 21:36:28
;; * Bug fix which use flex-autopair by mistake.
;;
;; Revision 1.5 2012/04/20 22:24:26
;; * Bug fix when just after string.
;; * Change some default settings.
;;
;; Revision 1.4.1 2012/04/04 21:05:48
;; * Bug fix for first key in c-mode and other modes.
;;
;; Revision 1.4 2012/04/03 20:15:21
;; * Regard first key as key-combo-execute-original when first key is not assigned
;; * Auto indent when inserting string have new line
;;
;; Revision 1.3 2012/03/13 22:00:23
;; * Make works well for other elisp which use post command hook
;;
;; Revision 1.2 2012/02/10 22:15:52
;; * Add support to use SKK. Bug reported by ballforest
;; * Bug fix for html mode.
;;
;; Revision 1.1 2012/02/08 21:56:27
;; * Add key-combo-define-local function to set key for local keymap.
;; * Add a lot of default setting in pogin's blog.
;;
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

(defun key-combo-lookup (key)
  ;; copy from `key-binding'
  "Return the binding for command KEY in key-combo keymaps.
KEY is a string or vector, a sequence of keystrokes.
The binding is probably a symbol with a function definition."
  (let ((string (key-description (vconcat key))))
    (key-binding (vector 'key-combo (intern string)))))

(defun key-combo-execute-orignal ()
  (interactive)
  (call-interactively (key-binding (this-command-keys-vector)))
  )

;; should be replace by union
(defun key-combo-memq (a b)
  (setq a (if (consp a) a (list a)))
  (setq b (if (consp b) b (list b)))
  (apply
   'append
   (delete-if
    'null
    (mapcar
     (lambda (x) (if (memq x b) (list x) nil))
     a))))

;; From context-skk.el
;; http://openlab.ring.gr.jp/skk/skk/main/context-skk.el
(defun key-combo-in-stringp ()
  (nth 3 (parse-partial-sexp (point) (point-min))))

(defun key-combo-in-commentp ()
  (nth 4 (parse-partial-sexp (point) (point-min))))

(defun key-combo-comment-or-stringp ()
  (if (or (key-combo-in-stringp) (key-combo-in-commentp))
      t
    nil))

;;(browse-url "http://q.hatena.ne.jp/1226571494")
(defun key-combo-count-boundary (last-undo-list)
  (length (remove-if-not 'null last-undo-list)))

(defun key-combo-execute-macro (string)
  (cond
   ((string-match "`!!'" string)
    (destructuring-bind (pre post) (split-string string "`!!'")
      (key-combo-execute-macro pre)
      (save-excursion
        (key-combo-execute-macro post))
      ))
   (t
    (let ((p (point)))
      (if (and (eq ?  (char-before))
               (eq ?  (aref string 0)))
          (delete-backward-char 1))
      (insert string)
      (when (string-match "\n" string)
        (indent-according-to-mode)
        (indent-region p (point)))))))

(defun key-combo-get-command(command)
  (unless (key-combo-elementp command)
    (error "%s is not command" command))
  (cond
   ((functionp command) command)
   ((listp command) command)
   ((not (stringp command)) nil)
   (t
    (lexical-let ((command command))
      (lambda()
        (key-combo-execute-macro command)
        )))
   );;end cond
  )

(defun key-combo-elementp (element)
  (or (functionp element)
      (stringp element)
      (null element));;for unset key
  )

(defun key-combo-define (keymap key commands)
  "In KEYMAP, define key sequence KEY as COMMANDS.
KEYMAP is a keymap.\n
KEY is a string or a vector of symbols and characters meaning a
sequence of keystrokes and events.  Non-ASCII characters with codes
above 127 (such as ISO Latin-1) can be included if you use a vector.\n
COMMANDS can be an interactive function, a string, nil, or list of these COMMAND.
If COMMANDS is string, treated as a smartchr flavor keyboard macro.
If COMMANDS is nil, the key-chord is removed.
If COMMANDS is list, treated as sequential commands.
"
  ;;copy from key-chord-define
  (let ((base-key (list (car (listify-key-sequence key)))))
  (cond
   ;;for sequence '(" = " " == ")
   ((and (not (key-combo-elementp commands))
         (key-combo-elementp (car-safe commands)))
      (let ((seq-keys base-key));;list
      (mapc '(lambda(command)
               (key-combo-define keymap (vconcat seq-keys) command)
               (setq seq-keys
                     (append seq-keys base-key)))
            commands)))
   (t
    (unless (key-combo-elementp commands)
      (error "%s is not command" commands))
    ;; regard first key as key-combo-execute-orignal
    (let ((first (key-binding
                  (vector 'key-combo (intern (key-description base-key))))))
      (when
          (and (eq (safe-length (listify-key-sequence key)) 2)
               (null first))
        (define-key keymap
          (vector 'key-combo (intern (key-description base-key)))
          'key-combo-execute-orignal)))
    (define-key keymap
      (vector 'key-combo (intern (key-description key)))
      (key-combo-get-command commands))
    ))))

(defun key-combo-define-global (keys command)
  "Give KEY a global binding as COMMAND.\n
See also `key-combo-define'\n
Note that if KEY has a local binding in the current buffer,
that local binding will continue to shadow any global binding
that you make with this function.
"
  ;;(interactive "sSet key chord globally (2 keys): \nCSet chord \"%s\" to command: ")
  (key-combo-define (current-global-map) keys command))

(defun key-combo-define-local (keys command)
  "Give KEY a local binding as COMMAND.\n
See also `key-combo-define'\n
The binding goes in the current buffer's local map,
which in most cases is shared with all other buffers in the same major mode.
"
  ;;(interactive "sSet key chord globally (2 keys): \nCSet chord \"%s\" to command: ")
  (key-combo-define (current-local-map) keys command))

;; < { [ should use flex-autopair
(defvar key-combo-global-default
  '(;; instead of using (goto-char (point-min))
    ;; use beginning-of-buffer for keydescription
    ("C-a"   . (back-to-indentation move-beginning-of-line
                                  beginning-of-buffer key-combo-return))
    ("C-e"   . (move-end-of-line end-of-buffer key-combo-return))
    ))

(defvar key-combo-lisp-default
  '(("."  . " . ")
    (","  . (key-combo-execute-orignal))
    (",@" . " ,@");; for macro
    (";"  . (";; " ";;; " "; "))
    (";=" . ";=> ")
    ("="  . ("= " "eq " "equal "))
    (">=" . ">= ")
    ("C-M-x" . (key-combo-execute-orignal
                (lambda ()
                  (let ((current-prefix-arg '(4)))
                    (call-interactively 'eval-defun)))));; lamda for message
    ("-"  . (key-combo-execute-orignal));; for symbol name
    ;; ("/" . ("/`!!'/" "/* `!!' */") );;for regexp, comment
    ))

(defvar key-combo-lisp-mode-hooks
  '(lisp-mode-hook
    emacs-lisp-mode-hook
    lisp-interaction-mode-hook
    inferior-gauche-mode-hook
    scheme-mode-hook))

(defmacro define-key-combo-load (name)
  "define-key-combo-load is deprecated"
  `(defun ,(intern (concat "key-combo-load-" name "-default")) ()
     (dolist (key ,(intern (concat "key-combo-" name "-default")))
       (key-combo-define-local (read-kbd-macro (car key)) (cdr key)))
     ))

;; for algol like language
(defcustom key-combo-common-mode-hooks
  '(c-mode-common-hook;; It's run immediately before the language specific hook.
    php-mode-hook
    ruby-mode-hook
    cperl-mode-hook
    javascript-mode-hook
    js-mode-hook
    js2-mode-hook
    )
  "Hooks that enable `key-combo-common-default' setting")

;; (browse-url "http://bojovs.github.com/2012/04/24/ruby-coding-style/")
(defcustom key-combo-common-default
  '((","  . ", ")
    ("="  . (" = " " == " " === " ));;" === " for js
    ("=>" . " => ")
    ("=~" . " =~ ");;for ruby regexp
    ("=*" . " =* ")
    ("+"  . (" + " "++"))
    ("+=" . " += ")
    ("-"  . (" - " "--"));undo when unary operator
    ("-=" . " -= ")
    ("->" . " -> ");; for haskell,coffee script. overwrite in c
    (">"  . (key-combo-execute-orignal " >> "))
    ;; " > " should be bind in flex-autopair
    (">=" . " >= ")
    (">>=" . " >>= ")
    ("%"  . " % ")
    ("%="  . " %= ")
    ("^"  . " ^ ");; XOR for c
    ("^="  . " ^= ");; for c
    ("!" . key-combo-execute-orignal)
    ;; NOT for c
    ;; don't use " !" because of ruby symbol
    ;; and unary operator
    ("!="  . " != " ) ;;" !== " for js and php
    ("!==" . " !== ") ;;" !== " for js and php
    ("!~" . " !~ ")   ; for ruby
    ("~" . key-combo-execute-orignal)
    ;; for unary operator
    ("::" . " :: ") ;; for haskell
    ;; (":" . ":");;for ruby symbol
    ("&"  . (" & " " && "))             ;overwrite in c
    ("&=" . " &= ");; for c
    ("&&=" . " &&= ")                   ; for ruby
    ("*"  . " * " )                     ;overwrite in c
    ("*="  . " *= " )
    ("**"  . "**" )                     ;for power
    ("**=" . " **=" )                     ;for power
    ;; ("?" . "? `!!' :"); ternary operator should be bound in yasnippet?
    ;; ("?=");; for coffeescript?
    ("<" . (key-combo-execute-orignal " << "))
    ;; " < " should be bound in flex-autopair
    ("<=" . " <= ")
    ;; ("<?" . "<?`!!'?>");; for what?
    ("<<=" . " <<= ");; bit shift for c
    ("<-" . " <- ")
    ("<!" . "<!-- `!!' -->");; for html comment
    ("|"  . (" | " " || "));; bit OR and OR for c
    ;;ToDo: ruby block
    ("|=" . " |= ");; for c
    ("||=" . " ||= ")                   ; for ruby
    ;; ("/" . (" / " "// " "/`!!'/")) ;; devision,comment start or regexp
    ("/" . (" / " "// "))
    ("/=" . " /= ")
    ("*/" . "*/")
    ("/*" . "/* `!!' */")
    ("/* RET" . "/*\n`!!'\n*/");; add *? m-j
    ;; ("/* RET" . "/*\n*`!!'\n*/");; ToDo:change style by valiable
    ("{" . (key-combo-execute-orignal))
    ("{ RET" . "{\n`!!'\n}")
    )
  "Default binding which enabled by `key-combo-common-mode-hooks'")

(defvar key-combo-org-default
  '(("C-a" . (org-beginning-of-line
             beginning-of-buffer
             key-combo-return));;back-to-indentation
    ("C-e" . (org-end-of-line
              end-of-buffer
              key-combo-return))
    ))

(defcustom key-combo-pointer-default
  '(("*" . ("*" "**" "***"))
    ("&" . ("&" "&&" "&&&"))
    ("->" . "->"))
  "Default binding for c-mode,c++-mode,objc-mode"
  )

;;;###autoload
(defmacro key-combo-define-hook (hooks name keys)
    `(progn
       (defun ,(nth 1 name) ()
         (key-combo-load-default-1 (current-local-map) ,keys)
         )
       (key-combo-load-by-hooks ,hooks ,name)
       ))

;;;###autoload
(defun key-combo-load-default ()
  (interactive)
  (key-combo-mode 1)
  (key-combo-load-default-1 (current-global-map)
                            key-combo-global-default)
  (key-combo-define-hook key-combo-common-mode-hooks
                         'key-combo-common-load-default
                         key-combo-common-default)
  (key-combo-define-hook key-combo-lisp-mode-hooks
                           'key-combo-lisp-load-default
                           key-combo-lisp-default)
  (key-combo-define-hook '(c-mode-hook c++-mode-hook)
                         'key-combo-pointer-load-default
                         key-combo-pointer-default)
  (key-combo-define-hook 'objc-mode-hook
                         'key-combo-objc-load-default
                         (append key-combo-pointer-default
                                 '(("@"  . "@\"`!!'\""))))
  (key-combo-define-hook 'org-mode-hook
                         'key-combo-org-load-default
                         key-combo-org-default)
  (key-combo-define-hook '(html-mode-hook
                           css-mode-hook
                           javascript-mode-hook
                           js-mode-hook
                           makefile-mode-hook
                           js2-mode-hook)
                         'key-combo-property-default
                         '((":"  . ": ")))
  ;; align is better for property?
  )

;; hooks function-name keys
(defun key-combo-load-by-hooks (hooks func)
  (let ((hooks (if (consp hooks) hooks (list hooks))))
    (dolist (hook hooks)
      (add-hook hook func t))
      ))

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

(defun key-combo-test-command-loop ()
  ;; (key-combo-finalize)
  (font-lock-fontify-buffer)
  (setq last-command nil)
  ;; (setq last-command 'self-insert-command)
  (while unread-command-events
    ;; (setq last-command-event (read-event))
    (let ;((key (read-event)))
      ((key (read-key-sequence-vector "a")))
      (font-lock-fontify-buffer)
      (flet ((this-command-keys-vector () key))
        (setq this-command (key-binding key))
        (setq last-command-event (aref key (1- (length key))))
        (funcall 'key-combo-pre-command-function)
        ;; (message "%S:%S" unread-command-events (this-command-keys-vector))
        (call-interactively this-command)
        (undo-boundary)
        (setq last-command this-command)
        ;; (message "th:%S k:%s nu:%s" this-command key key-combo-need-undop)
        )
      )
    ;; (message "%S:%S" unread-command-events last-command-event)
    )
  ;;(key-binding "C-M-x")
  (setq key-combo-command-keys nil)
  )

(defun key-combo-test-command (mode command)
  (with-temp-buffer
    (funcall mode)
    (setq unread-command-events (listify-key-sequence (read-kbd-macro command)))
    (key-combo-test-command-loop)
    (buffer-substring-no-properties (point-min) (point-max))
  ))

(dont-compile
  (when(fboundp 'expectations)
    (expectations
      (expect ".."
        (key-combo-test-command 'ruby-mode ".."))
      (expect "..."
        (key-combo-test-command 'ruby-mode "..."))
      (expect " !~ "
        (key-combo-test-command 'ruby-mode "!~"))
      (expect "**"
        (key-combo-test-command 'ruby-mode "**"))
      (expect " ||= "
        (key-combo-test-command 'ruby-mode "||="))
      (expect "\"\" . \n"
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "\"\"\n")
          (goto-char 3)
          (setq unread-command-events (listify-key-sequence (kbd ".")))
          (key-combo-test-command-loop)
          (buffer-substring-no-properties (point-min) (point-max))
          ))
      (expect "\"\" . a"
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "\"\"a")
          (goto-char 3)
          (setq unread-command-events (listify-key-sequence (kbd ".")))
          (key-combo-test-command-loop)
          (buffer-substring-no-properties (point-min) (point-max))
          ))
      (expect "\"\" . "
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "\"\"")
          (setq unread-command-events (listify-key-sequence (kbd ".")))
          (key-combo-test-command-loop)
          (buffer-substring-no-properties (point-min) (point-max))
          ))
      (expect "\".\""
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "\"\"")
          (goto-char 2)
          (setq unread-command-events (listify-key-sequence (kbd ".")))
          (key-combo-test-command-loop)
          (buffer-substring-no-properties (point-min) (point-max))
          ))
      (expect "a . \"\""
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "a\"\"")
          (goto-char 2)
          (setq unread-command-events (listify-key-sequence (kbd ".")))
          (key-combo-test-command-loop)
          (buffer-string)
          ))
      (expect "*"
        (with-temp-buffer
          (c-mode)
          (setq unread-command-events (listify-key-sequence (kbd "*")))
          (key-combo-test-command-loop)
          (buffer-string)
          ))
      (expect "->"
        (with-temp-buffer
          (c-mode)
          (setq unread-command-events (listify-key-sequence (kbd "->")))
          (key-combo-test-command-loop)
          (buffer-string)
          ))
    (desc "RET")
      (expect "/*\n  \n */"
        (with-temp-buffer
          ;; (buffer-enable-undo);;
          (c-mode)
          (setq unread-command-events (listify-key-sequence (kbd "/* RET")))
          (key-combo-test-command-loop)
          (buffer-string)
          ))
      (expect "{\n  \n}"
        (with-temp-buffer
          ;; (buffer-enable-undo);;
          (c-mode)
          (setq unread-command-events (listify-key-sequence (kbd "{ RET")))
          (key-combo-test-command-loop)
          (buffer-string)
          ))
      (desc "key-combo")
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
      (expect nil
        (with-temp-buffer
          (emacs-lisp-mode)
          ;; (insert "\"")
          (font-lock-fontify-buffer)
          (key-combo-comment-or-stringp)
          ))
      (expect t
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "\"")
          (font-lock-fontify-buffer)
          (key-combo-comment-or-stringp)
          ))
      (expect t
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert ";")
          (font-lock-fontify-buffer)
          (key-combo-comment-or-stringp)
          ))
      (expect nil
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert ";\n")
          (font-lock-fontify-buffer)
          (key-combo-comment-or-stringp)
          ))
      (expect "= "
        (with-temp-buffer
          (emacs-lisp-mode)
          (font-lock-fontify-buffer)
          (setq unread-command-events (listify-key-sequence
                                       (kbd "=")))
          (key-combo-test-command-loop)
          (buffer-substring-no-properties (point-min) (point-max))
          ))
      (desc "isearch-mode")
      (expect "";;bug? "="
        (with-temp-buffer
          (emacs-lisp-mode)
          (isearch-mode t)
          (font-lock-fontify-buffer)
          (setq unread-command-events (listify-key-sequence
                                       (kbd "=")))
          (key-combo-test-command-loop)
          (isearch-done)
          (buffer-substring-no-properties (point-min) (point-max))
          ;; (setq isearch-mode nil)
          ))
      (expect "\"="
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "\"")
          (font-lock-fontify-buffer)
          (setq unread-command-events (listify-key-sequence
                                       (kbd "=")))
          (key-combo-test-command-loop)
          (buffer-substring-no-properties (point-min) (point-max))
          ))
      (expect ";="
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert ";")
          (font-lock-fontify-buffer)
          (setq unread-command-events (listify-key-sequence
                                       (kbd "=")))
          (key-combo-test-command-loop)
          (buffer-substring-no-properties (point-min) (point-max))
          ))
      (expect "= "
        (with-temp-buffer
          (emacs-lisp-mode)
          (setq key-combo-command-keys nil)
          (font-lock-fontify-buffer)
          (setq unread-command-events (listify-key-sequence
                                       (kbd "=")))
          (key-combo-test-command-loop)
          (buffer-substring-no-properties (point-min) (point-max))
          ))
      (expect ","
        (with-temp-buffer
          (setq unread-command-events (listify-key-sequence
                                       (kbd ",")))
          (key-combo-test-command-loop)
          (buffer-substring-no-properties (point-min) (point-max))
          ))
      (expect ",,"
        (with-temp-buffer
          (setq unread-command-events (listify-key-sequence
                                       (kbd ",,")))
          (key-combo-test-command-loop)
          (buffer-substring-no-properties (point-min) (point-max))
          ))
      (expect " . "
        (with-temp-buffer
          (emacs-lisp-mode)
          (setq unread-command-events (listify-key-sequence
                                       (kbd ".")))
          (key-combo-test-command-loop)
          (buffer-substring-no-properties (point-min) (point-max))
          ))
      (expect ";; ."
        (with-temp-buffer
          (emacs-lisp-mode)
          (setq unread-command-events (listify-key-sequence
                                       (kbd ";.")))
          (key-combo-test-command-loop)
          (buffer-substring-no-properties (point-min) (point-max))
          ))
      (expect ";; ,"
        (with-temp-buffer
          (emacs-lisp-mode)
          (buffer-enable-undo)
          (setq unread-command-events (listify-key-sequence
                                       (kbd ";,")))
          (key-combo-test-command-loop)
          (buffer-substring-no-properties (point-min) (point-max))
          ))
      (desc "for skk")
      (expect ";,"
        (with-temp-buffer
          (emacs-lisp-mode)
          (setq this-command 'self-insert-command)
          (insert ";")
          (font-lock-fontify-buffer)
          (setq last-command-event ?,)
          (key-combo-pre-command-function)
          (call-interactively this-command)
          (buffer-substring-no-properties (point-min) (point-max))
          ))
      (expect ";、"
        (with-temp-buffer
          (emacs-lisp-mode)
          (skk-mode 1)
          (setq this-command 'skk-insert)
          (insert ";")
          (setq unread-command-events (listify-key-sequence
                                       (kbd ",")))
          (key-combo-test-command-loop)
          (buffer-substring-no-properties (point-min) (point-max))
          ))
      (expect ";。"
        (with-temp-buffer
          (emacs-lisp-mode)
          (skk-mode 1)
          (setq this-command 'skk-insert)
          (insert ";")
          (setq unread-command-events (listify-key-sequence
                                       (kbd ".")))
          (key-combo-test-command-loop)
          (buffer-substring-no-properties (point-min) (point-max))
          ))
      (desc "comment or string")
      (expect ";."
        (with-temp-buffer
          (emacs-lisp-mode)
          (setq this-command 'self-insert-command)
          (insert ";")
          (font-lock-fontify-buffer)
          (setq unread-command-events (listify-key-sequence
                                       (kbd ".")))
          (key-combo-test-command-loop)
          (buffer-substring-no-properties (point-min) (point-max))
          ))
      (expect ";\n;; "
        (with-temp-buffer
          (emacs-lisp-mode)
          (setq this-command 'self-insert-command)
          (insert ";\n")
          (font-lock-fontify-buffer)
          (setq unread-command-events (listify-key-sequence
                                       (kbd ";")))
          (key-combo-test-command-loop)
          (buffer-substring-no-properties (point-min) (point-max))
          ))
      (expect ">"
        (with-temp-buffer
          (setq this-command 'self-insert-command)
          (setq unread-command-events (listify-key-sequence
                                       (kbd ">")))
          (key-combo-test-command-loop)
          (buffer-substring-no-properties (point-min) (point-max))
          ))
      (expect "="
        (with-temp-buffer
          (buffer-enable-undo)
          (setq unread-command-events (listify-key-sequence
                                       (kbd "=")))
          (key-combo-test-command-loop)
          (buffer-substring-no-properties (point-min) (point-max))
          ))
      (expect " = "
        (with-temp-buffer
          (c-mode)
          (buffer-enable-undo)
          (setq unread-command-events (listify-key-sequence
                                       (kbd "=")))
          (key-combo-test-command-loop)
          (buffer-substring-no-properties (point-min) (point-max))
          ))
      (expect " =* "
        (with-temp-buffer
          (c-mode)
          (buffer-enable-undo)
          (setq unread-command-events (listify-key-sequence
                                       (kbd "=*")))
          (key-combo-test-command-loop)
          (buffer-substring-no-properties (point-min) (point-max))
          ))
      (desc "sequence")
      (expect " == "
        (with-temp-buffer
          (c-mode)
          (buffer-enable-undo)
          (setq unread-command-events (listify-key-sequence
                                       (kbd "==")))
          (key-combo-test-command-loop)
          (buffer-substring-no-properties (point-min) (point-max))
          ))
      (expect " => "
        (with-temp-buffer
          (c-mode)
          (buffer-enable-undo);;todo remove
          (setq unread-command-events (listify-key-sequence
                                       (kbd "=>")))
          (key-combo-test-command-loop)
          (buffer-substring-no-properties (point-min) (point-max))
          ))
      (expect " === "
        (with-temp-buffer
          (c-mode)
          (buffer-enable-undo);;todo remove
          (setq unread-command-events (listify-key-sequence
                                       (kbd "===")))
          (key-combo-test-command-loop)
          (buffer-substring-no-properties (point-min) (point-max))
          ))
      (expect 'eval-last-sexp
        (with-temp-buffer
          (key-combo-finalize)
          (insert "\"a\"")
          (setq this-command 'eval-last-sexp)
          (setq unread-command-events (listify-key-sequence
                                       (kbd "C-x C-e")))
          (key-combo-test-command-loop)
          this-command
          ))
      (expect "I"
        (with-temp-buffer
          (key-combo-finalize)
          (insert "B\n IP")
          (setq unread-command-events (listify-key-sequence
                                       (kbd "C-a")))
          (key-combo-test-command-loop)
          (char-to-string(following-char))
          ))
      (expect "I"
        (with-temp-buffer
          (emacs-lisp-mode)
          (font-lock-fontify-buffer)
          (key-combo-finalize)
          (insert "B\n I;P")
          (setq unread-command-events (listify-key-sequence
                                       (kbd "C-a")))
          (key-combo-test-command-loop)
          (char-to-string(following-char))
          ))
      (expect " "
        (with-temp-buffer
          (key-combo-finalize)
          (insert "B\n IP")
          (setq unread-command-events (listify-key-sequence
                                       (kbd "C-a C-a")))
          (key-combo-test-command-loop)
          (char-to-string(following-char))
          ))
      (expect "B"
        (with-temp-buffer
          (key-combo-finalize)
          (insert "B\n IP")
          (setq unread-command-events (listify-key-sequence
                                       (kbd "C-a C-a C-a")))
          (key-combo-test-command-loop)
          (char-to-string(following-char))
          ))
      (expect "P"
        (with-temp-buffer
          (key-combo-finalize)
          (insert "B\n IP")
          (backward-char)

          (char-to-string(following-char))
          ))
      ;;(mock (edebug-defun) :times 1);;=> nil
      ;;(mock (expectations-eval-defun) :times 1);;=> nil
      (expect (mock (test1 *) :times 1);;=> nil
        (with-temp-buffer
          (key-combo-define-global (kbd "M-C-d") '(test1 test2))
          (setq unread-command-events (listify-key-sequence
                                       (kbd "M-C-d")))
          (key-combo-test-command-loop)
          ))
      ;; "\M-\C-d"
      (expect (mock (test2 *) :times 1);;=> nil
        (with-temp-buffer
          ;; (message "execute mock")
          (key-combo-define-global (kbd "M-C-d") '(test1 test2))
          (setq unread-command-events (listify-key-sequence
                                       (kbd "M-C-d M-C-d")))
          (key-combo-test-command-loop)
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
      (expect '("aa" 2)
        (with-temp-buffer
          (funcall (key-combo-get-command "a`!!'a"))
          (list (buffer-string) (point))
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
      ;; (expect (mock (define-key * * *) :times 2);;(not-called define-key)
      ;;   ;;(mock   (define-key * * *) :times 0);;=> nil
      ;;   (stub key-binding =>'key-combo)
      ;;   (key-combo-define-local "a" '("a" "bb"))
      ;;   )
      (desc "undo")
      (expect "="
        (with-temp-buffer
          (c-mode)
          (buffer-enable-undo)
          (setq this-command 'self-insert-command)
          (setq unread-command-events (listify-key-sequence "="))
          (key-combo-test-command-loop)
          (undo);;
          (buffer-string)
          ))
      (expect " = "
        (with-temp-buffer
          (c-mode)
          (buffer-enable-undo);;
          (setq unread-command-events (listify-key-sequence "=="))
          (setq this-command 'self-insert-command)
          (key-combo-test-command-loop)
          (undo);;
          (buffer-substring-no-properties (point-min) (point-max))
          ))
      (desc "loop")
      (expect " = "
        (with-temp-buffer
          (c-mode)
          (buffer-enable-undo);;
          (setq unread-command-events (listify-key-sequence "===="))
          (key-combo-test-command-loop)
          (buffer-string)
          ))
      (expect " => = "
        (with-temp-buffer
          (c-mode)
          (buffer-enable-undo);;
          (setq unread-command-events (listify-key-sequence "=>="))
          (key-combo-test-command-loop)
          (buffer-string)
          ))
      (desc "key-combo-lookup")
      (expect " = "
        (with-temp-buffer
          (c-mode)
          (funcall
           (key-combo-lookup "="))
          (buffer-string)))
      (expect " == "
        (with-temp-buffer
          (c-mode)
          (funcall
           (key-combo-lookup "=="))
          (buffer-string)))
      (expect " == "
        (with-temp-buffer
          (c-mode)
          (key-combo-define-global (kbd "C-M-h") " == ")
          (funcall
           (key-combo-lookup (kbd "C-M-h")))
          (buffer-string)))
      (expect " === "
        (with-temp-buffer
          (c-mode)
          (key-combo-define-global (kbd "C-M-h C-M-h") " === ")
          (buffer-enable-undo)
          (setq unread-command-events
                (listify-key-sequence "\C-\M-h\C-\M-h"))
          (key-combo-test-command-loop)
          (buffer-string)))
      (expect " === "
        (with-temp-buffer
          (c-mode)
          (key-combo-define-global (kbd "C-M-h C-M-h") " === ")
          (funcall
           (key-combo-lookup (kbd "C-M-h C-M-h")))
          (buffer-string)))
      (expect " = "
        (with-temp-buffer
          (c-mode)
          (funcall
           (key-combo-lookup [?=]))
          (buffer-string)))
      (expect " == "
        (with-temp-buffer
          (c-mode)
          (funcall
           (key-combo-lookup [?= ?=]))
          (buffer-string)))
      (expect " => "
        (with-temp-buffer
          (c-mode)
          (funcall
           (key-combo-lookup [?= ?>]))
          (buffer-string)))
      (expect " === "
        (with-temp-buffer
          (c-mode)
          (funcall
           (key-combo-lookup [?= ?= ?=]))
          (buffer-string)))
      (expect nil
        (key-combo-lookup [?= ?= ?= ?=]))
      (desc "key-combo-elementp")
      (expect t
        (every 'identity
        ;; (identity
               (mapcar (lambda(command)
                         (progn (key-combo-define-global ">>" command)
                                (identity (key-combo-lookup ">>"))))
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

(defun key-combo-undo ()
  "returns buffer undo list"
  ;; (message "count:%d" (1+ (key-combo-count-boundary buffer-undo-list)))
  (primitive-undo (1+ (key-combo-count-boundary buffer-undo-list))
                  buffer-undo-list)
  )

(defun key-combo-command-execute (command)
  "returns buffer undo list"
    (cond
     ((commandp command)
      (call-interactively command))
     (t (funcall command)))
    (undo-boundary)
  )

(defvar key-combo-command-keys nil)
(defvar key-combo-need-undop t)

(defun key-combo ()
  ;; because of prefix arg
  (interactive)
  (let ((command (key-combo-lookup key-combo-command-keys)))
    (if (and key-combo-need-undop
             (not (eq buffer-undo-list t)))
        (key-combo-undo)
      )
    (key-combo-command-execute command)
    (setq key-combo-need-undop t)
    ))

(defvar key-combo-original-undo-list nil)

(defun key-combo-finalize ()
  (if (not (eq buffer-undo-list t))
      (setq buffer-undo-list
            (append buffer-undo-list key-combo-original-undo-list)))
  (setq key-combo-original-undo-list nil)
  (setq key-combo-command-keys nil)
  )

(defun key-combo-pre-command-function ()
  (let ((command-key-vector (this-command-keys-vector))
        (first-timep (not (eq last-command 'key-combo))))
    (setq key-combo-command-keys
          ;; use last-command-event becase of testability
          (vconcat key-combo-command-keys command-key-vector))
    (unless (key-combo-lookup key-combo-command-keys);;retry
      ;; need undo?
      (if (and (not (eq 2 (length key-combo-command-keys)))
               (equal [] (delete (aref key-combo-command-keys 0)
                                 key-combo-command-keys)))
          (setq key-combo-need-undop t)
        (setq key-combo-need-undop nil)
        ;; (setq first-timep t)
        )
      (setq key-combo-command-keys command-key-vector))
    (cond ((and
            key-combo-mode
            (not (minibufferp))
            (not isearch-mode)
            (key-combo-lookup key-combo-command-keys)
            (not (and (key-combo-comment-or-stringp)
                      (memq (key-binding command-key-vector)
                            '(self-insert-command skk-insert))))
            )
           (setq this-command 'key-combo)
           (cond (first-timep
                  ;; first time
                  (setq key-combo-original-undo-list buffer-undo-list
                        buffer-undo-list nil)
                  (key-combo-set-start-position (cons (point) (window-start)))
                  (cond ((memq (key-binding command-key-vector)
                               '(self-insert-command skk-insert))
                           (undo-boundary)
                           (key-combo-command-execute
                            (key-binding
                             command-key-vector))
                           (setq key-combo-need-undop t)
                           ;; )
                         ));;;
                  )
                 ;; continue
                 ((eq key-combo-need-undop nil)
                  ;; finalize
                  (if (not (eq buffer-undo-list t))
                      (setq key-combo-original-undo-list
                            (append buffer-undo-list
                                    key-combo-original-undo-list)))
                  (setq buffer-undo-list nil)
                  ;; (setq key-combo-command-keys nil)
                  )
                 ))
          (t
           (if (eq last-command 'key-combo)
               (key-combo-finalize)
             ))
          )))

;;;###autoload
(define-minor-mode key-combo-mode
  "Toggle key combo."
  :lighter " KC"
  :global t
  :group 'key-combo
  (if key-combo-mode
      (add-hook 'pre-command-hook
                ;;post-self-insert-hook
                #'key-combo-pre-command-function)
    (remove-hook 'pre-command-hook
                 #'key-combo-pre-command-function))
  )

;; (listify-key-sequence
;;  (kbd "M-C-d M-C-d"))
;; (listify-key-sequence
;;  "\M-\C-d\M-\C-d")
;; (append
;;  (kbd "M-C-d M-C-d") nil)
;; (append
;;  "\M-\C-d\M-\C-d" nil);; not expected!!
;; ;; (vconcat
;; ;;  "\M-\C-d\M-\C-d")
;; (event-convert-list '(control meta ?a))
;;; (local-set-key "\M-\C-d" 'hoge)

;;todo filter
;; filter for mode
;; filter for inside string ""
;; filter for inside comment ;;

;; copy from terminal
;; xterm
;; http://ttssh2.sourceforge.jp/manual/ja/usage/tips/vim.html
;; http://d.hatena.ne.jp/guyon/20090224/1235485381
;; Bracketed Paste Mode
;; http://togetter.com/li/289305
;; http://www.bookshelf.jp/texi/elisp-manual/21-2-8/jp/elisp_40.html#SEC654
;; http://shyouhei.tumblr.com/post/63240207/pos-command-hook

;; support lamda func
(provide 'key-combo)
;;; key-combo.el ends here
