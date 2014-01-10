;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; anything
(require 'anything-startup)

;; anything でコマンド検索
(defalias 'read-command (symbol-function 'anything-read-command))
(substitute-key-definition 'execute-extended-command 'anything-execute-extended-command global-map)

(setq anything-sources (list anything-c-source-buffers
                             anything-c-source-bookmarks
                             anything-c-source-file-name-history
                             anything-c-source-locate
                             anything-c-source-mac-spotlight))
(define-key anything-map (kbd "C-p") 'anything-previous-line)
(define-key anything-map (kbd "C-n") 'anything-next-line)
(define-key anything-map (kbd "C-v") 'anything-next-source)
(define-key anything-map (kbd "M-v") 'anything-previous-source)

(global-set-key "\C-x\C-l" 'anything)
