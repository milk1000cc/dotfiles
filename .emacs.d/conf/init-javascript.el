;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; js2-mode
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.es6$" . js2-jsx-mode))
(custom-set-variables '(js2-basic-offset 2))
(setq js2-strict-missing-semi-warning nil)
(setq js2-include-node-externs t)
(setq-default js2-global-externs '("gon"))
