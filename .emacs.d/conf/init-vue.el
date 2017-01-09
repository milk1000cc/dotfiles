;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; vue-mode
(defun my-vue-mode-hook ()
  "Hooks for vue-mode."
  (setq indent-tabs-mode nil)
  (setq tab-width 2))
(add-hook 'vue-mode-hook 'my-vue-mode-hook)

;; vue-mode uses js-mode (not js2-mode)
;; https://github.com/CodeFalling/vue-mode#why-js-mode-instead-of-js2-mode
(setq js-indent-level 2)

(setq mmm-submode-decoration-level 0)

(require 'vue-mode)
