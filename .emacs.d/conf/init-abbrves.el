;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

(setq save-abbrevs nil)

;; 動的略語展開
(define-key global-map "\C-o" 'dabbrev-expand)

;; dabbrev時に、大文字・小文字を区別しない
(setq dabbrev-case-fold-search nil)

;; dabbrevをいろんな単語にマッチさせる
(setq dabbrev-abbrev-char-regexp "[A-z0-9:-]")
