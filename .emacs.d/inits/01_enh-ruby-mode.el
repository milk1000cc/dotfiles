;; 深いインデントを避ける
(setq enh-ruby-deep-indent-paren nil)

;; マジックコメントを入れない
(setq enh-ruby-add-encoding-comment-on-save nil)

;; 120 文字目以降を目立たせる
;; http://d.hatena.ne.jp/kitokitoki/20100802/p1
(defun highlight-too-long-line ()
  "Highlight too long line."
  (font-lock-add-keywords
   nil
   '(("^[^\n]\\{120\\}\\(.*\\)$" 1 font-lock-warning-face t))))
(add-hook 'enh-ruby-mode-hook 'highlight-too-long-line)

;; Ruby のファイルが enh-ruby-mode で開くように
(add-to-list 'auto-mode-alist
             '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\|Brew\\)file\\)\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.god\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.cap\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.jb\\'" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
