;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; js2-mode
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-jsx-mode))
(custom-set-variables '(js2-basic-offset 2))
(setq js2-strict-missing-semi-warning nil)
