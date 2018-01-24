;; web-modeのインデント設定用フック
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2) ; HTMLのインデイント
  (setq web-mode-css-indent-offset 2) ; CSSのインデント
  (setq web-mode-code-indent-offset 2) ; JS, PHP, Rubyなどのインデント
  (setq web-mode-comment-style 2) ; web-mode内のコメントのインデント
  (setq web-mode-style-padding 1) ; <style>内のインデント開始レベル
  (setq web-mode-script-padding 1) ; <script>内のインデント開始レベル
  )
(add-hook 'web-mode-hook 'my-web-mode-hook)

;; Thanks to: http://cortyuming.hateblo.jp/entry/2015/01/17/113525
(defun my-web-mode-highlight-ignore-str ()
  (interactive "p")
  (highlight-regexp "\t" 'hi-yellow)
  (highlight-regexp "　" 'hi-pink)
  (highlight-regexp "[ \t]+$" 'hi-blue)
  )
(add-hook 'web-mode-hook 'my-web-mode-highlight-ignore-str)

(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
