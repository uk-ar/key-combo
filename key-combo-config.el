;;; key-combo-config.el --- key-combo additional configuations

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

;;; Commentary:

;;

;;; Code:

;; < { [ should use flex-autopair
(defvar key-combo-global-default
  '(;; instead of using (goto-char (point-min))
    ;; use beginning-of-buffer for keydescription
    ("C-a"   . (back-to-indentation move-beginning-of-line
                                    beginning-of-buffer key-combo-return))
    ("C-e"   . (end-of-line end-of-buffer key-combo-return))
    ))

(defvar key-combo-lisp-default
  '(("."  . (key-combo-execute-original))
    (". SPC" . " . ")
    ("SPC"  . (key-combo-execute-original))
    ("SPC ." . " . ")
    (","  . (key-combo-execute-original))
    (",@" . " ,@");; for macro
    ;; (";"  . ";; ")
    (";"  . (";; " ";;; " "; ")) ;cannot use because of comment
    ;; (";=" . ";=> ")
    ("="  . ("= " "eq " "equal "))
    (">=" . ">= ")
    ("C-M-x" . (key-combo-execute-original
                (lambda ()
                  (interactive)
                  (let ((current-prefix-arg '(4)))
                    (call-interactively 'eval-defun)))));; lamda for message
    ("-"  . (key-combo-execute-original));; for symbol name
    ;; ("/" . ("/`!!'/" "/* `!!' */") );;for regexp, comment
    ))

(defvar key-combo-lisp-mode-hooks
  '(lisp-mode-hook
    emacs-lisp-mode-hook
    lisp-interaction-mode-hook
    inferior-gauche-mode-hook
    scheme-mode-hook))

(defun key-combo-read-kbd-macro (start)
  (when (or (equal (elt start 0) ?\ )
            (equal (elt start (1- (length start))) ?\ ))
    ;; (error "To bind the key SPC, use \" \", not [SPC]")
    (error "To bind the key SPC, use SPC, not \" \""))
  (read-kbd-macro start))

(defmacro define-key-combo-load (name)
  "define-key-combo-load is deprecated"
  `(defun ,(intern (concat "key-combo-load-" name "-default")) ()
     (dolist (key ,(intern (concat "key-combo-" name "-default")))
       (key-combo-define-local (key-combo-read-kbd-macro (car key)) (cdr key)))
     ))

;; for algol like language
(defcustom key-combo-common-mode-hooks
  '(c-mode-common-hook;; It's run immediately before the language specific hook.
    php-mode-hook
    ruby-mode-hook
    cperl-mode-hook
    perl-mode-hook
    python-mode-hook
    javascript-mode-hook
    js-mode-hook
    js2-mode-hook
    )
  "Hooks that enable `key-combo-common-default' setting"
  :group 'key-combo)

;; (browse-url "http://bojovs.github.com/2012/04/24/ruby-coding-style/")
(defcustom key-combo-common-default
  '((","  . ", ")
    ("="  . (" = " " == " " === " ));;" === " for js
    ("=>" . " => ")
    ("=~" . " =~ ");;for ruby regexp
    ("=*" . " =* ")                     ;for c
    ("+"  . (" + " "++"))
    ("+=" . " += ")
    ("-"  . (" - " "--"))               ;undo when unary operator
    ("-=" . " -= ")
    ("->" . " -> ");; for haskell,coffee script. overwrite in c
    (">"  . (key-combo-execute-original " >> "))
    ;; " > " should be bind in flex-autopair
    (">=" . " >= ")
    (">>=" . " >>= ")
    ("%"  . " % ")
    ("%="  . " %= ")
    ("^"  . " ^ ");; XOR for c
    ("^="  . " ^= ");; for c
    ("!" . key-combo-execute-original)
    ;; NOT for c
    ;; don't use " !" because of ruby symbol
    ;; and unary operator
    ("!="  . " != " ) ;;" !== " for js and php
    ("!==" . " !== ") ;;" !== " for js and php
    ("!~" . " !~ ")   ; for ruby
    ("~" . key-combo-execute-original)
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
    ("<" . (key-combo-execute-original " << "))
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
    ("/" . (key-combo-execute-original))
    ("/ SPC" . " / ")
    ("/=" . " /= ")
    ("*/" . "*/")
    ("/*" . "/* `!!' */")
    ("/* RET" . "/*\n`!!'\n*/");; add *? m-j
    ;; ("/* RET" . "/*\n*`!!'\n*/");; ToDo:change style by valiable
    ("{" . (key-combo-execute-original))
    ("{ RET" . "{\n`!!'\n}")
    )
  "Default binding which enabled by `key-combo-common-mode-hooks'"
  :group 'key-combo)

(defcustom key-combo-org-default
  '(("C-a" . (org-beginning-of-line
              beginning-of-buffer
              key-combo-return));;back-to-indentation
    ("C-e" . (org-end-of-line
              end-of-buffer
              key-combo-return))
    )
  "Default binding which enabled by `org-mode-hook'"
  :group 'key-combo)

(defcustom key-combo-pointer-default
  '(("*" . ("*" "**" "***"))
    ("&" . ("&" "&&" "&&&"))
    ("->" . "->"))
  "Default binding for c-mode,c++-mode,objc-mode"
  :group 'key-combo)

(defcustom key-combo-perl-default
  '(("$" . (key-combo-execute-original))
    ("@" . (key-combo-execute-original))
    ("%" . (key-combo-execute-original))
    ("&" . (key-combo-execute-original))
    ("*" . (key-combo-execute-original))
    ("->" . "->"))
  "Default binding for c-mode,c++-mode,objc-mode"
  :group 'key-combo)

;;;###autoload
(defmacro key-combo-define-hook (hooks name keys)
  ;; fix me:name to real name (not symbol)
  ;; :hooks to quote
  ;; don't use macro?
  `(progn
     (defun ,(nth 1 name) ()
       (key-combo-load-default-1 (current-local-map) ,keys)
       )
     (key-combo-load-by-hooks ,hooks ,name)
     ))

;;;###autoload
(defun key-combo-load-default ()
  (interactive)
  (global-key-combo-mode t)
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
  (key-combo-define-hook '(cperl-mode-hook perl-mode-hook)
                         'key-combo-pointer-load-default
                         key-combo-perl-default)
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
    (key-combo-define map (key-combo-read-kbd-macro (car key)) (cdr key))))

(provide 'key-combo-config)
;;; key-combo-config.el ends here
