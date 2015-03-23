[![Build Status](https://secure.travis-ci.org/uk-ar/key-combo.png)](http://travis-ci.org/uk-ar/key-combo)

# key-combo.el

*key-combo* is an Emacs package that provides "cycling" key-binding. Multiple
commands are executed sequentially with a repeated keypress of a single
key. Besides single key, a whole cords of keys could be bound to a command in
order to create complex "smart operators".


## Instalation


This package is available from [MELPA](http://melpa.org/#/). Alternatively you
can place `key-combo.el` into your emacs path and add `(require 'key-combo)` to
your init file.


## Activation


Activate `key-combo-mode` localy in a mode hook with:

```lisp
(key-combo-mode 1)
```

or globally with

```lisp
(global-key-combo-mode t)
```

## Configuration

Define cycling key combos globally with `key-combo-define-global` function:

```lisp
(key-combo-define-global "=" '(" = " " == " " === " )) ;cycling
(key-combo-define-global "=>" " => ")
(key-combo-define-global "C-a" `(back-to-indentation move-beginning-of-line
                                 beginning-of-buffer key-combo-return))
(key-combo-define-global "C-e" '(move-end-of-line end-of-buffer key-combo-return))
```

or for each modes separately:

```lisp
(key-combo-define emacs-lisp-mode-map "="  '("= " "eq " "equal "))
```

There is also `key-combo-define-local` that should be used inside mode hooks.


To facilitate quick declaration of cord, `key-combo.el` provides
`key-combo-define-hook` which is used to setup same hook for a list of
modes. Here is a simple example for `emacs-lisp`:

```lisp

(defvar my-lisp-mode-hooks
  '(lisp-mode-hook
    emacs-lisp-mode-hook
    lisp-interaction-mode-hook
    inferior-gauche-mode-hook
    scheme-mode-hook))

(defvar my-key-combos-for-lisp
  '(("."  . ("." " . "))
    (","  . (key-combo-execute-original))
    (",@" . " ,@")
    (";=" . ";=> ")
    ("="  . ("= " "eq " "equal "))
    (">=" . ">= ")))

(key-combo-define-hook my-lisp-mode-hooks ; hooks
                       'my-key-combo-lisp-hook ; function name
                       my-key-combos-for-lisp)

```

## Default Configuration

To load a range of default configurations for `lisp`, `C`, `C++`, `js`, `org`
etc modes, use:

```lisp
(key-combo-load-default)
````

See the code for details.

```

## Related Projects

`key-combo` is intended as a superset of the following projects:

- [smartchr.el](https://github.com/imakado/emacs-smartchr)
- [key-cord.el](http://emacswiki.org/emacs/key-chord.el)
- [electric-spacing.el](https://github.com/xwl/electric-spacing) (old [smart-operator.el](http://www.emacswiki.org/emacs/SmartOperator))

Anything you can do with the above package you should be able to do with
`key-combo`, and much more.
