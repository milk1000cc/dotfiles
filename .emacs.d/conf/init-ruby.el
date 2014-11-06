;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; 深いインデントを避ける
(setq ruby-deep-indent-paren-style nil)

;; 80 文字目以降を目立たせる
;; http://d.hatena.ne.jp/kitokitoki/20100802/p1
(add-hook 'ruby-mode-hook
  (lambda ()
    (font-lock-add-keywords nil
      '(("^[^\n]\\{80\\}\\(.*\\)$" 1 font-lock-warning-face t)))))

(add-to-list 'auto-mode-alist '("\\.god$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.cap$" . ruby-mode))

;; マジックコメントを入れない
(setq ruby-insert-encoding-magic-comment nil)

;; インデントをちゃんとする
;; http://stackoverflow.com/questions/19900180/emacs-24-ruby-mode-indentation-behavior-when-using-iterator-such-as-each
(setq ruby-use-smie nil)
