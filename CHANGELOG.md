# Changelog

## 1.6 2015/03/24 15:24:37

* eldoc and company-mode work correctly with key-combo commands
* Partial fix for multiple-cursors
* Several IME fixes and improvements
* Support vector format in definition of key-combos
* Add global-key-combo-mode-map to support global keys
* Enable the use of SPC key
* Add key-combo-disable-modes to disable in some modes

## 1.5.1 2012/06/06 21:36:28
* Bug fix which use flex-autopair by mistake.

## 1.5 2012/04/20 22:24:26
* Bug fix when just after string.
* Add !== for js and php's not triple-equal by tomykaira.
* Change some default settings.

## 1.4.1 2012/04/04 21:05:48
* Bug fix for first key in c-mode and other modes.

## 1.4 2012/04/03 20:15:21
* Regard first key as key-combo-execute-original when first key is not assigned
* Auto indent when inserting string have new line

## 1.3 2012/03/13 22:00:23
* Make works well for other elisp which use post command hook

## 1.2 2012/02/10 22:15:52
* Add support to use SKK. Bug reported by ballforest
* Bug fix for html mode.

## 1.1 2012/02/08 21:56:27
* Add key-combo-define-local function to set key for local keymap.
* Add a lot of default setting in pogin's blog.

## 1.0 2012/01/31 22:03:50
* Change clean-up function to use undo

## 0.7 2012/01/17 21:25:10
* Insert white space dwim

## 0.6 2012/01/16 21:17:01
* Allow cleanup function as nil
* Add key-combo-return function,
which can move to point of command beginning.
* Allow meta key for key-combo key.
* Save undo history when self-insert-command.

## 0.5 2012/01/13 23:02:39
* Support function as key-combo command

## 0.4
* Map key to minor mode to toggle enable and disable.

## 0.3
* Not to cleanup when 1 sequence key
* Bugfix by tomykaira
* Refactoring
* Add test cases

## 0.2
* First release

## 0.1
* Initial revision

