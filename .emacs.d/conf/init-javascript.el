;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; js2-mode
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.es6$" . js2-mode))
(custom-set-variables '(js2-basic-offset 2))
