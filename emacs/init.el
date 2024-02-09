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

;; デフォルトのメジャーモードを text-mode に
(setq-default major-mode 'text-mode)

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
              ("e" . wdired-change-to-wdired-mode))
  :init
  (setq dired-free-space nil))

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
  ;; enh-ruby-mode, js2-mode では、長い列も強調
  (defun my/set-whitespace-style-including-lines-tail ()
    (setq-local whitespace-style '(face trailing tabs spaces lines-tail)))
  (add-hook 'enh-ruby-mode-hook 'my/set-whitespace-style-including-lines-tail)
  (add-hook 'js2-mode-hook 'my/set-whitespace-style-including-lines-tail)
  (add-hook 'text-mode-hook
            (lambda ()
              ;; コミットメッセージの編集画面を見やすく
              (when (and (buffer-file-name) (string-match-p "COMMIT_EDITMSG$" (buffer-file-name)))
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
  (setq helm-move-to-line-cycle-in-source nil)
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

;; enh-ruby-mode
(use-package enh-ruby-mode
  :mode
  "\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'"
  "Brewfile\\'" "\\.builder\\'"
  :interpreter "ruby"
  :init
  (setq enh-ruby-deep-indent-paren nil)
  :custom-face
  (enh-ruby-string-delimiter-face ((t (:foreground "red"))))
  (enh-ruby-regexp-delimiter-face ((t (:foreground "red"))))
  (enh-ruby-heredoc-delimiter-face ((t (:foreground "red"))))
  (enh-ruby-op-face ((t (:foreground "#ccc"))))
  (enh-ruby-regexp-face ((t (:foreground "red")))))

;; js2-mode
(use-package js2-mode
  :mode "\\.js\\'" "\\.gs\\'"
  :init
  (setq js2-global-externs '("URL" "IntersectionObserver" "ResizeObserver" "sessionStorage"))
  (setq js2-include-node-externs t)
  (setq js2-strict-missing-semi-warning nil))

;; json-mode
(use-package json-mode
  :init
  (setq js-indent-level 2))

;; sass-mode
(use-package sass-mode
  :mode "\\.sss\\'"
  :config
  (add-hook 'sass-mode-hook
            (lambda ()
              (setq-local comment-start "//"))))

;; dotenv-mode
(use-package dotenv-mode
  :mode "\\.envrc\\'" "\\.envrc\\.example\\'")

;; sh-script
(use-package sh-script
  :init
  (setq sh-basic-offset 2)
  (setq sh-shell-file "/usr/bin/env sh"))

;; systemd
(use-package systemd
  :mode (("\\.service\\.erb\\'" . systemd-mode)
         ("\\.socket\\.erb\\'" . systemd-mode)))

;; mmm-mode
(use-package mmm-mode
  :custom-face
  (mmm-default-submode-face ((t (:background nil)))))

;; flycheck
(use-package flycheck
  :hook (json-mode . flycheck-mode))

;; projectile-rails
(use-package projectile-rails
  :bind ("C-c C-p" . my/rails-toggle-between-controller-and-view)
  :config
  (defun rails/ruby/current-method ()  ; https://github.com/dmexe/emacs-rails-reloaded/blob/master/rails-ruby.el#L31
    (let (action
          (re "^ *def +\\([^ (\n]+\\)"))
      (save-excursion
        (end-of-line)
        (when (re-search-backward re nil t)
          (setq action (buffer-substring-no-properties (match-beginning 1) (match-end 1)))))
      action))
  (defun my/rails-toggle-between-controller-and-view ()
    (interactive)
    (if (string-match-p "app/controllers/.+\\.rb$" (buffer-file-name))
        (let ((current-method (rails/ruby/current-method)))
          (projectile-rails-find-current-resource "app/views/"
                                                  "^${plural}/\\(${current-method}\..+\\)$"
                                                  'projectile-rails-find-current-view))
      (projectile-rails-find-current-controller))))

(use-package dockerfile-mode)
(use-package markdown-mode)
(use-package nginx-mode)
(use-package pug-mode)
(use-package recentf-ext)
(use-package rspec-mode)
(use-package slim-mode)
(use-package vue-mode)
(use-package yaml-mode)
