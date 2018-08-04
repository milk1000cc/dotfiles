(require 'helm-config)

;; helm-mini のソース
(setq helm-mini-default-sources '(helm-source-buffers-list
                                  helm-source-recentf
                                  helm-source-locate))

;; カスタムキーバインド
(define-key global-map (kbd "C-x C-l") 'helm-mini)
(define-key global-map (kbd "M-x") 'helm-M-x)
