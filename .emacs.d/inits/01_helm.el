(require 'helm-config)

;; helm + projectile
(require 'helm-projectile)
(setq projectile-completion-system 'helm)

;; helm-mini のソース
(setq helm-mini-default-sources '(helm-source-buffers-list
                                  helm-source-recentf
                                  helm-source-projectile-files-list
                                  helm-source-locate))

;; カスタムキーバインド
(define-key global-map (kbd "C-x C-l") 'helm-mini)
(define-key global-map (kbd "M-x") 'helm-M-x)
(define-key global-map (kbd "M-y") 'helm-show-kill-ring)
