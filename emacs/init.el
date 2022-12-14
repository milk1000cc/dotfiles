;; straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; スタートアップ時のメッセージを抑制
(setq inhibit-startup-message t)

;; メニューバーを非表示
(menu-bar-mode 0)

;; ビープ音を抑制
(setq ring-bell-function 'ignore)

;; バックアップしない
(setq make-backup-files nil)

;; 自動保存しない
(setq auto-save-list-file-prefix nil)
(setq auto-save-default nil)

;; ロックファイルを作成しない
(setq create-lockfiles nil)

;; 列数を表示
(column-number-mode t)

;; yes/no を y/n で選択できるように
(defalias 'yes-or-no-p 'y-or-n-p)

;; kill-line で行末の改行文字も削除
(setq kill-whole-line t)

;; インデントはスペースで
(setq-default indent-tabs-mode nil)

;; 最終行に空行を追加する
(setq require-final-newline t)

;; シンボリックリンク先がバージョン管理されていても問題ない
(setq vc-follow-symlinks t)

;; C-j でインデント
(electric-indent-mode 0)

;; 対応する括弧をハイライト表示
(show-paren-mode t)

;; 更新されたファイルを自動的に読み込み直す
(global-auto-revert-mode t)

;; 略称は保存しない
(setq save-abbrevs nil)

;; recentf
(setq recentf-max-saved-items 10000)
(setq recentf-auto-cleanup 30)
(setq recentf-auto-save-timer
      (run-with-idle-timer 30 t 'recentf-save-list))

;; cua-mode (CUA キーバインドは無効)
(setq cua-enable-cua-keys nil)
(cua-mode t)

;; コマンド履歴の保存
(setq history-delete-duplicates t)
(savehist-mode 1)

;; フェイス
(custom-set-faces
 '(default ((t (:background "#111" :foreground "#ccc"))))
 '(font-lock-builtin-face ((t (:foreground "brightblue"))))
 '(font-lock-comment-face ((t (:foreground "red"))))
 '(font-lock-constant-face ((t (:foreground "cyan"))))
 '(font-lock-function-name-face ((t (:foreground "brightblue"))))
 '(font-lock-keyword-face ((t (:foreground "brightblue"))))
 '(font-lock-string-face ((t (:foreground "red"))))
 '(font-lock-type-face ((t (:foreground "green"))))
 '(font-lock-variable-name-face ((t (:foreground "red"))))
 '(mode-line ((t (:background "#333" :foreground "#ccc"))))
 '(mode-line-inactive ((t (:background "#ccc" :foreground "#333"))))
 '(region ((t (:background "#666")))))

;; カスタムキーバインド
(bind-key "C-x C-i" 'indent-region)
(bind-key "C-u" 'undo)
(bind-key "C-x l" 'goto-line)
(bind-key "C-o" 'dabbrev-expand)
(bind-key "C-x SPC" 'cua-set-rectangle-mark)
(bind-key* "C-h" 'delete-backward-char)

;; dired
(use-package dired
  :straight nil
  :bind (:map dired-mode-map
              ("e" . wdired-change-to-wdired-mode)))

;; whitespace
(use-package whitespace
  :init
  (setq whitespace-space-regexp "\\(\u3000+\\)")
  (setq whitespace-style '(face trailing tabs spaces))  ; 基本は、行末スペース・タブ・ハードスペース・全角スペースを強調
  (setq whitespace-line-column 120)
  :custom-face
  (whitespace-trailing ((t (:background nil :foreground "SteelBlue" :underline t))))
  (whitespace-tab ((t (:background "Gray"))))
  (whitespace-hspace ((t (:background "Gray"))))
  (whitespace-space ((t (:background "Aquamarine"))))
  (whitespace-line ((t (:background nil :foreground "brightred"))))
  :config
  (global-whitespace-mode 1)
  (add-hook 'ruby-mode-hook
            (lambda ()
              ;; ruby-mode では、長い列も強調
              (setq-local whitespace-style '(face trailing tabs spaces lines-tail))))
  (add-hook 'text-mode-hook
            (lambda ()
              ;; コミットメッセージの編集画面を見やすく
              (when (string-match "COMMIT_EDITMSG$" (buffer-file-name))
                (setq-local whitespace-style '(face trailing spaces))))))

;; rg
(use-package rg
  :init
  (setq rg-command-line-flags '("--hidden"))
  (setq rg-group-result nil)
  (setq rg-show-columns t)
  (setq rg-show-header nil)
  (setq transient-save-history nil)
  :custom-face
  (rg-info-face ((t :underline t)))
  :config
  ;; result buffer を都度生成する (https://rgel.readthedocs.io/en/2.2.1/usage.html#command-rg-save-search)
  (defadvice rg-run (before rg-run-before activate)
    (rg-save-search))

  (rg-define-search my/rg-literal
    :format literal
    :files "everything"
    :flags ("--no-ignore"))
  (rg-define-search my/rg-regexp
    :files "everything"
    :flags ("--no-ignore"))
  (rg-define-search my/rg-project-literal
    :format literal
    :files "everything"
    :flags ("-g '!.git/'")  ; https://github.com/BurntSushi/ripgrep/discussions/1578
    :dir project)
  (rg-define-search my/rg-project-regexp
    :files "everything"
    :flags ("-g '!.git/'")
    :dir project)
  (defalias 'rg 'my/rg-literal)
  (defalias 'rg-regexp 'my/rg-regexp)
  (defalias 'rg-project 'my/rg-project-literal)
  (defalias 'rg-project-regexp 'my/rg-project-regexp))

;; wgrep
(use-package wgrep
  :init
  (setq wgrep-auto-save-buffer t))

;; helm
(use-package helm
  :bind (("C-x C-l" . helm-mini)
         ("M-x" . helm-M-x)
         ("C-x ?" . helm-apropos))
  :init
  (setq helm-mini-default-sources '(helm-source-buffers-list
                                    helm-source-recentf
                                    helm-source-projectile-files-list))
  (setq savehist-additional-variables '(extended-command-history))
  :custom-face
  (helm-buffer-directory ((t (:background nil :foreground "brightblue"))))
  (helm-ff-directory ((t (:background nil :foreground "LightSteelBlue"))))
  (helm-ff-file ((t (:inherit 'helm-ff-directory))))
  (helm-ff-symlink ((t (:inherit 'helm-ff-directory))))
  :config
  (use-package helm-projectile))

;; helm-bundle-show
(use-package helm-bundle-show
  :config
  (defalias 'bundle-show 'helm-bundle-show))

;; xclip
(use-package xclip
  :config
  (xclip-mode 1))

;; ruby-mode
(use-package ruby-mode
  :mode "Brewfile\\'" "\\.builder\\'"
  :init
  (setq ruby-insert-encoding-magic-comment nil))

;; js2-mode
(use-package js2-mode
  :mode "\\.js\\'" "\\.json\\'" "\\.gs\\'"
  :init
  (setq js2-strict-missing-semi-warning nil)
  (setq js2-global-externs '("URL"))
  :config
  (setq js2-basic-offset 2))

;; dotenv-mode
(use-package dotenv-mode
  :mode "\\.envrc\\'" "\\.envrc\\.example\\'")

;; systemd
(use-package systemd
  :mode (("\\.service\\.erb\\'" . systemd-mode)
         ("\\.socket\\.erb\\'" . systemd-mode)))

;; mmm-mode
(use-package mmm-mode
  :custom-face
  (mmm-default-submode-face ((t (:background nil)))))

;; text-mode
(use-package text-mode
  :straight nil
  :mode "COMMIT_EDITMSG\\'")

;; projectile
(use-package projectile
  :bind ("C-c , t" . projectile-toggle-between-implementation-and-test)
  :init
  (setq projectile-create-missing-test-files t))

;; projectile-rails
(use-package projectile-rails
  :bind ("C-c C-p" . my/rails-toggle-between-controller-and-view)
  :config
  (defun my/rails-toggle-between-controller-and-view ()
    (interactive)
    (if (string-match "app/controllers/.+\\.rb$" (buffer-file-name))
        (projectile-rails-find-current-view)
      (projectile-rails-find-current-controller))))

(use-package dockerfile-mode)
(use-package lua-mode)
(use-package markdown-mode)
(use-package nginx-mode)
(use-package pug-mode)
(use-package recentf-ext)
(use-package sass-mode)
(use-package slim-mode)
(use-package vue-mode)
(use-package yaml-mode)
