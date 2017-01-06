;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; web-mode
;; http://umi-uyura.hatenablog.com/entry/2015/05/13/214629
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-attr-indent-offset nil)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-sql-indent-offset 2)
  (setq indent-tabs-mode nil)
  (setq tab-width 2))
(add-hook 'web-mode-hook 'my-web-mode-hook)

(add-to-list 'auto-mode-alist '("\\.html\\.erb$" . web-mode))

;; Thanks to: http://cortyuming.hateblo.jp/entry/2015/01/17/113525
 (defun my-web-mode-highlight-ignore-str ()
    (interactive "p")
    (highlight-regexp "\t" 'hi-yellow)
    (highlight-regexp "　" 'hi-pink)
    (highlight-regexp "[ \t]+$" 'hi-blue)
    )
  (add-hook 'web-mode-hook 'my-web-mode-highlight-ignore-str)
