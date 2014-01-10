;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; emacs-rails
(add-to-load-path "~/.emacs.d/elisp/rails-minor-mode/")
(eval-when-compile (require 'cl))
(require 'rails)

(define-key rails-minor-mode-map "\C-c\C-p" 'rails-lib:run-primary-switch)
(define-key rails-minor-mode-map "\C-c\C-n" 'rails-lib:run-secondary-switch)
(define-key rails-minor-mode-map "\C-c\C-cl" 'rails-log:open)
