;;; key-combo.el --- map key sequence to commands -*- lexical-binding: t -*-
;; 
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
;; 
;; Author: Yuuki Arisawa <yuuki.ari@gmail.com>
;; Maintainer: Vitalie Spinu <spinuvit@gmail.com>
;; URL: https://github.com/uk-ar/key-combo
;; Created: 30 November 2011
;; Version: 2.0
;; Keywords: keyboard input

;;; Commentary:
;; 
;; ########   Compatibility   ########################################
;;
;; Works with Emacs-23.2.1, 23.1.1
;; 
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

;; Code

(require 'cl)

(defcustom key-combo-disable-modes nil
  "Major modes `key-combo-mode' can not run on."
  :group 'key-combo)

(defvar kc-command-keys nil)
(defvar kc-need-undo t)
(defvar key-combo-original-undo-list nil)
;; fixme: temporary workaround. Don't activate when multiple-cursor mode is on.
(defvar multiple-cursors-mode nil)


;;; UTILS

(defun kc-in-string-p ()
  (nth 3 (syntax-ppss)))

(defun kc-in-comment-p ()
  (nth 4 (syntax-ppss)))

(defun kc-in-string-or-comment-p ()
  (let ((ppss (syntax-ppss)))
    (or (nth 3 ppss)
	(nth 4 ppss))))

(defun key-combo-count-boundary (last-undo-list)
  (length (remove-if-not 'null last-undo-list)))

(defun key-combo-undo ()
  "returns buffer undo list"
  ;; (message "count:%d" (1+ (key-combo-count-boundary buffer-undo-list)))
  (primitive-undo (1+ (key-combo-count-boundary buffer-undo-list))
                  buffer-undo-list))


;;; CORE

(defun key-combo-describe ()
  "List key combo bindings in a help buffer."
  (interactive)
  (describe-bindings [key-combo]))

(defun kc-make-key-vector (key)
  (vector 'key-combo
          ;; "_" is for error when key is " "
          (intern (concat "_" (key-description (vconcat key))))))

(defun key-combo-key-binding (key)
  "Return the binding for command KEY in current key-combo keymap.
KEY is a sequence of keystrokes (a string or a vector)."
  (key-binding (kc-make-key-vector key)))

(defun key-combo-lookup-key (keymap key)
  "Return the binding for command KEY in key-combo keymaps.
KEY is a string or vector, a sequence of keystrokes.
The binding is probably a symbol with a function definition."
    (lookup-key keymap (kc-make-key-vector (vconcat key))))

(defun key-combo-read-kbd-macro (start)
  (when (or (equal (elt start 0) ?\ )
            (equal (elt start (1- (length start))) ?\ ))
    ;; (error "To bind the key SPC, use \" \", not [SPC]")
    (error "To bind the key SPC, use SPC, not \" \""))
  (read-kbd-macro start))

(defun kc-get-command (command)  
  (cond
   ((functionp command) command)
   ((stringp command) command)
   ((null command) nil)
   ;; ((listp command) command)
   (t (error "%s is not a valid key combo command" command))))

(defun kc-element-p (element)
  (or (functionp element)
      (stringp element)
      ;;for unset key
      (null element)))

(defun key-combo-pre-command-function ()
  ;;(dbg kc-command-keys)
  (when (and key-combo-mode
	     (not (minibufferp))
	     (not isearch-mode)
 	     (not multiple-cursors-mode))
    (let ((cmd-keys (this-command-keys-vector))
	  (firstp (not (eq last-command 'key-combo))))
      (setq kc-command-keys
	    ;; use last-command-event becase of testability
	    (vconcat kc-command-keys cmd-keys))
      (unless (key-combo-key-binding kc-command-keys);;cycle
	;;(dbg "here1")
	;; need undo?
	(setq kc-need-undo
	      (and (not (eq 2 (length kc-command-keys)))
		   (equal [] (delete (aref kc-command-keys 0)
				     kc-command-keys))))
	(setq kc-command-keys cmd-keys))
      ;;(dbg kc-command-keys)
      ;;(dbg kc-need-undo)
      (cond ((and (key-combo-key-binding kc-command-keys)
		  (not (and (kc-in-string-or-comment-p)
			    (memq (key-binding cmd-keys)
				  '(self-insert-command skk-insert)))))
	     (setq this-command 'key-combo)
	     ;;(dbg (key-combo-key-binding kc-command-keys))
	     (cond (firstp
		    (setq key-combo-original-undo-list buffer-undo-list
			  buffer-undo-list nil)
		    (key-combo-set-start-position (cons (point) (window-start)))
		    (when (memq (key-binding cmd-keys)  '(self-insert-command skk-insert))
		      ;;(dbg "undo-boundary")
		      (undo-boundary)
		      (key-combo-execute (key-binding cmd-keys))
		      (setq kc-need-undo t)))
		   ;; continue
		   ((eq kc-need-undo nil)
		    ;;(dbg "need-undo nil")
		    ;; finalize
		    (unless (eq buffer-undo-list t)
		      (setq key-combo-original-undo-list
			    (append buffer-undo-list
				    key-combo-original-undo-list)))
		    (setq buffer-undo-list nil))))
	    (t
	     (when (eq last-command 'key-combo)
	       ;;(dbg "finalize")
	       (key-combo-finalize)))))))

;; fixme: are these declaration really needed? 
(declare-function key-combo-set-start-position "key-combo")
(declare-function key-combo-return "key-combo")
 
;;(declare-function key-combo-return "")
(lexical-let ((key-combo-start-position nil))
  (defun key-combo-set-start-position (pos)
    (setq key-combo-start-position pos))
;;;###autoload  
  (defun key-combo-return ()
    "Return to the position when sequence of calls of the same command was started."
    (unless (eq key-combo-start-position nil)
      (progn
        ;; (set-window-start (selected-window) (cdr key-combo-start-position))
        (goto-char (car key-combo-start-position))))))


;;; EXECUTIONS

(defun key-combo-execute-original ()
  (interactive)
  (call-interactively (key-binding (this-command-keys-vector))))

(defun key-combo-execute-macro (string)
  (cond
   ;; fixme: use | like in smartparents
   ((string-match "`!!'" string)
    (destructuring-bind (pre post) (split-string string "`!!'")
      (key-combo-execute-macro pre)
      (save-excursion
        (key-combo-execute-macro post))))
   (t
    (let ((p (point)))
      ;; fixme: use just-one-space
      (if (and (eq ?  (char-before))
               (eq ?  (aref string 0)))
          (delete-char -1))
      (insert string)
      ;; fixme: should be an option
      (when (string-match "\n" string)
        (indent-according-to-mode)
        (indent-region p (point)))))))

(defun key-combo ()
  ;; because of prefix arg
  (interactive)
  (let ((command (key-combo-key-binding kc-command-keys)))
    (if (and kc-need-undo
             (not (eq buffer-undo-list t)))
        (key-combo-undo))
    (key-combo-execute command)
    (setq kc-need-undo t)))

(defun key-combo-finalize ()
  (when (and (not (eq buffer-undo-list t))
	     key-combo-original-undo-list)
    (setq buffer-undo-list
	  (append buffer-undo-list key-combo-original-undo-list)))
  (setq key-combo-original-undo-list nil)
  (setq kc-command-keys nil))

(defun key-combo-execute (command)
  "returns buffer undo list"
  (cond
   ((stringp command)
    (key-combo-execute-macro command))
   ((commandp command)
    (call-interactively command))
   ((functionp command)
    (funcall command))
   (t (error "%s is not a key-combo command" command)))
  (undo-boundary))


;;; USER LEVEL

;;;###autoload
(defun key-combo-define (keymap key commands)
  "In KEYMAP, define key sequence KEY as COMMANDS.
KEY is a string or a vector of symbols and characters
representing a sequence of keystrokes or events.  Use vectors to
input Non-ASCII characters with codes above 127 (such as ISO
Latin-1).  COMMANDS can be an interactive function, a string,
nil, or list of these COMMAND.

If COMMANDS is nil the key-combo is removed. When COMMANDS is a
string it is inserted as is. If COMMANDS is list, it is treated
as sequential commands."
  ;;copy from key-chord-define
  (let ((base-key (list (car (listify-key-sequence key)))))
    (cond
     ;;for sequence '(" = " " == ")
     ((and (listp (kc-element-p commands))
           (kc-element-p (car-safe commands)))
      (let ((seq-keys base-key))
        (mapc (lambda (cmd)
		(key-combo-define keymap (vconcat seq-keys) cmd)
		(setq seq-keys (append seq-keys base-key)))
              commands)))
     ((kc-element-p commands)
      ;; regard first key as key-combo-execute-original
      ;; fixme: 1) do we really need this?
      ;; fixme: 2) make it work for longer sequences than 2 
      (let ((first (lookup-key keymap (kc-make-key-vector base-key))))
        (when (and (eq (safe-length (listify-key-sequence key)) 2)
		   (null first))
	  (define-key keymap
            (kc-make-key-vector base-key)
            'key-combo-execute-original)))
      (define-key keymap
	(kc-make-key-vector key)
	(kc-get-command commands)))
     (t
      (error "%s is not a valid command" commands)))))

;;;###autoload 
(defun key-combo-define-global (keys command)
  "Give KEY a global binding as COMMAND.
If KEY has a local binding in the current buffer, that local
binding will continue to shadow any global binding that you make
with this function.  See also `key-combo-define'."
  (key-combo-define (current-global-map) keys command))

;;;###autoload
(defun key-combo-define-local (keys command)
  "Give KEY a local binding as COMMAND.
The binding goes in the current buffer's local map, which in most
cases is shared with all other buffers in the same major mode.
See also `key-combo-define'"
  (key-combo-define (current-local-map) keys command))

(defun key-combo-define-kalist (map keys)
  (dolist (key keys)
    (key-combo-define map (key-combo-read-kbd-macro (car key)) (cdr key))))

;;;###autoload
(defmacro key-combo-define-hook (hooks name keys)
  ;; fixme: name is redundant
  `(progn
     (defun ,(nth 1 name) ()
       (key-combo-define-kalist (current-local-map) ,keys))
     (key--combo-load-by-hooks ,hooks ,name)))

;; fixme: move into the above macro
(defun key--combo-load-by-hooks (hooks func)
  (let ((hooks (if (consp hooks) hooks (list hooks))))
    (dolist (hook hooks)
      (add-hook hook func t))))

;;;###autoload
(define-minor-mode key-combo-mode
  "Toggle key combo."
  :lighter " KC"
  :group 'key-combo
  :keymap (make-sparse-keymap)
  (if key-combo-mode
      (add-hook 'pre-command-hook
                ;;post-self-insert-hook
                #'key-combo-pre-command-function nil t)
    (remove-hook 'pre-command-hook
                 #'key-combo-pre-command-function t)))

;; copy from auto-complete-mode-maybe
(defun key-combo-mode-maybe ()
  "What buffer `key-combo-mode' prefers."
  (when (and (not (minibufferp (current-buffer)))
             (not (memq major-mode key-combo-disable-modes))
             (key-combo-mode 1))))

;; copy from global-auto-complete-mode
;;;###autoload
(define-global-minor-mode global-key-combo-mode
  key-combo-mode key-combo-mode-maybe
  ;; :init-value t bug?
  :group 'key-combo)


;;; FIXES
(eval-after-load "eldoc"
  '(eldoc-add-command "key-combo"))

(eval-after-load "company"
  '(add-to-list 'company-begin-commands 'key-combo))

;; (key-combo-define-global (kbd "C-h C-b") " boo ")
;; (listify-key-sequence "abc")
;; (listify-key-sequence "==")
;; (listify-key-sequence (kbd "M-C-d M-C-d a"))
;; (listify-key-sequence "\M-\C-d\M-\C-d")
;; (append (kbd "M-C-d M-C-d") nil)
;; (append "\M-\C-d\M-\C-d" nil)
;; (vconcat "\M-\C-d\M-\C-d")
;; (event-convert-list '(control meta ?a))

;; copy from terminal
;; xterm
;; http://ttssh2.sourceforge.jp/manual/ja/usage/tips/vim.html
;; http://d.hatena.ne.jp/guyon/20090224/1235485381
;; Bracketed Paste Mode
;; http://togetter.com/li/289305
;; http://www.bookshelf.jp/texi/elisp-manual/21-2-8/jp/elisp_40.html#SEC654
;; http://shyouhei.tumblr.com/post/63240207/pos-command-hook

(provide 'key-combo)
;;; key-combo.el ends here
