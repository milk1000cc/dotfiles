;; 自動的にプロジェクト管理を開始
(projectile-mode)

;; プロジェクト情報をキャッシュする
(setq projectile-enable-caching t)

;; カスタムキーバインド
(define-key projectile-mode-map (kbd "C-c , t") 'projectile-toggle-between-implementation-and-test)
