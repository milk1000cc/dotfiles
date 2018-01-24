;; Thanks to:
;; http://techblog.kayac.com/emacs.html
;; http://syohex.hatenablog.com/entry/20121211/1355231365
;; http://biwakonbu.com/?p=569

(deftheme pastel
  "Pastel color theme")

(custom-theme-set-faces
 'pastel
 ;; 文字
 '(default ((t (:foreground "#CCCCCC" :background "#111111"))))

 ;; 選択範囲
 '(region ((t (:background "#666666"))))

 ;; モードライン
 '(mode-line ((t (:foreground "#CCCCCC" :background "#333333"))))
 '(mode-line-inactive ((t (:foreground "#333333" :background "#CCCCCC"))))

 ;; 組み込み関数名
 '(font-lock-builtin-face ((t (:foreground "brightblue"))))

 ;; 関数名
 '(font-lock-function-name-face ((t (:foreground "brightblue"))))

 ;; 変数
 '(font-lock-variable-name-face ((t (:foreground "red"))))

 ;; 文字列
 '(font-lock-string-face ((t (:foreground "red"))))

 ;; キーワード
 '(font-lock-keyword-face ((t (:foreground "brightblue"))))

 ;; シンボル
 '(font-lock-constant-face ((t (:foreground "cyan"))))

 ;; コメント
 '(font-lock-comment-face ((t (:foreground "red"))))

 ;; 定数・クラス
 '(font-lock-type-face ((t (:foreground "green"))))

 ;; 警告
 '(font-lock-warning-face ((t (:foreground "brightred"))))

 ;; helm
 '(helm-ff-file ((t (:foreground "LightSteelBlue"))))
 '(helm-ff-symlink ((t (:foreground "LightSteelBlue"))))

 ;; enh-ruby-mode
 '(enh-ruby-string-delimiter-face ((t (:foreground "red"))))
 '(enh-ruby-heredoc-delimiter-face ((t (:foreground "red"))))
 '(enh-ruby-regexp-delimiter-face ((t (:foreground "red"))))
 '(enh-ruby-regexp-face ((t (:foreground "red"))))
 '(enh-ruby-op-face ((t (:foreground "#CCCCCC"))))
 )

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'pastel)
