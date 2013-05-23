key-combo.el
========

[![Build Status](https://secure.travis-ci.org/uk-ar/key-combo.png)](http://travis-ci.org/uk-ar/key-combo)

;;
;; (require 'key-combo)
;; (key-combo-mode 1)
;;
;; and some chords, for example
;;
;; (key-combo-define-global (kbd "=") '(" = " " == " " === " ))

;; (key-combo-define-global (kbd "=>") " => ")
;;
;; or load default settings
;;
;; (key-combo-load-default)

;; Integrating multiple commands into one command is sometimes
;; useful. Pressing C-e at the end of line is useless and adding the
;; other behavior in this situation is safe.
;; 
;; For example, defining `my-end': if point is at the end of line, go
;; to the end of buffer, otherwise go to the end of line. Just evaluate it!
;;
;; (define-sequential-command my-end  end-of-line end-of-buffer)
;; (global-set-key "\C-e" 'my-end)
;;
;; Consequently, pressing C-e C-e is `end-of-buffer'!
;;
;; `define-sequential-command' is a macro that defines a command whose
;; behavior is changed by sequence of calls of the same command.
;;
;; `seq-return' is a command to return to the position when sequence
;; of calls of the same command was started.

*key-combo* is a Emacs plugin to support input several candidates with a single 
key, like ess-smart-underscore in Emacs Speaks Statistics -- when user types 
"_" key, it inserts " <- " for the first time or it replaces " <- " by "_" for 
the second time.  This plugin provides functions to support to define such 
key bindings easily.  For example: 

- <kbd>=</kbd>:insert ` = `
- <kbd>==</kbd>:insert ` == `
- <kbd>===</kbd>:insert ` === `
- <kbd>====</kbd>:insert ` = `

- <kbd>=</kbd>:insert ` = `
- <kbd>=></kbd>:insert ` => `

- <kbd>C-a</kbd>:move-beginning-of-line
- <kbd>C-a C-a</kbd>:beginning-of-buffer
- <kbd>C-a C-a C-a</kbd>:return

1st key|2nd key|3rd key|command
------|------|----|---
=   |||   ` = `
=|=|| ` == `
=|=|=| ` === `
=|>|| ` => `