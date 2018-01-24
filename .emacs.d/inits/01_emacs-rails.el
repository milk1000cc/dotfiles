(eval-when-compile (require 'cl))
(require 'rails)

;; C-c C-p で、コントローラとビューのファイル切り替え
(define-key rails-minor-mode-map (kbd "C-c C-p") 'rails-lib:run-primary-switch)
