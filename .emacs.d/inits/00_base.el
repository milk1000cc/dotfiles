;; スタートアップ時のメッセージを抑制
(setq inhibit-startup-message t)

;; メニューバーを非表示
(menu-bar-mode 0)

;; ビープ音を抑制
(setq ring-bell-function 'ignore)

;; バックアップしない
(setq make-backup-files nil)

;; 自動保存しない
(setq auto-save-default nil)

;; 列数を表示
(column-number-mode t)

;; yes/noを、y/nで選択できるようにする
(defalias 'yes-or-no-p 'y-or-n-p)

;; kill-lineで行末の改行文字も削除
(setq kill-whole-line t)

;; インデントはスペースで
(setq-default indent-tabs-mode nil)

;; 最終行に空行を追加する
(setq require-final-newline t)

;; シンボリックリンク先がバージョン管理されていても問題ない
(setq vc-follow-symlinks t)

;; C-j でインデント
(electric-indent-mode 0)

;; Mac Clipboard との共有
;; https://qiita.com/tstomoki/items/24d63217f797c6929a23
(defun copy-from-osx ()
 (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
 (let ((process-connection-type nil))
     (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
       (process-send-string proc text)
       (process-send-eof proc))))

(when (and (equal system-type 'darwin) (not window-system))
  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx))

;; 対応する括弧をハイライト表示
(show-paren-mode t)

;; 全角スペース、タブ等の様々な空白文字をハイライト
;; Meadow/memoからもらってきたと思われる。
(defface my-face-b-1 '((t (:background "medium aquamarine"))) nil)
(defface my-face-b-2 '((t (:background "gray"))) nil)
(defface my-face-u-1 '((t (:foreground "SteelBlue" :underline t))) nil)
(defvar my-face-b-1 'my-face-b-1)
(defvar my-face-b-2 'my-face-b-2)
(defvar my-face-u-1 'my-face-u-1)
(defadvice font-lock-mode (before my-font-lock-mode ())
  (font-lock-add-keywords
   major-mode
   '(
     ("　" 0 my-face-b-1 append)
     ("\t" 0 my-face-b-2 append)
     ("[ ]+$" 0 my-face-u-1 append)
     )))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)
(add-hook 'find-file-hooks '(lambda ()
                              (if font-lock-mode
                                  nil
                                (font-lock-mode t))))

;; 略称は保存しない
(setq save-abbrevs nil)

;; recentf
(setq recentf-max-saved-items 100000)
(setq recentf-exclude '("recentf"))
(setq recentf-auto-save-timer
      (run-with-idle-timer 30 t 'recentf-save-list))
(recentf-mode t)

;; cua-mode (CUA キーバインドは無効)
(cua-mode t)
(setq cua-enable-cua-keys nil)

;; カスタムファイルを別ファイルにする
(setq custom-file (locate-user-emacs-file "custom.el"))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)

;; テーマ
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'pastel t)

;; カスタムキーバインド
(require 'wdired)
(define-key global-map (kbd "C-x C-i") 'indent-region)
(define-key global-map (kbd "C-u") 'undo)
(define-key global-map (kbd "C-x l") 'goto-line)
(define-key global-map (kbd "C-o") 'dabbrev-expand)
(define-key global-map (kbd "C-x SPC") 'cua-set-rectangle-mark)
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
(define-key dired-mode-map "e" 'wdired-change-to-wdired-mode)

;; WSL の emacs で C-SPC が効かないので、F8 で代替
;; AutoHotkey などで C-SPC を F8 に割り当てる
(define-key global-map (kbd "<f8>") 'set-mark-command)
