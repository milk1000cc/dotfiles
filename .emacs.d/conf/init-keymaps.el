;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; C-h を BackSpaceに
(global-set-key "\C-h" 'delete-backward-char)

;; C-x C-i でリージョンをインデント
(global-set-key "\C-x\C-i" 'indent-region)

;; C-x l で goto-line
(define-key ctl-x-map "l" 'goto-line)
