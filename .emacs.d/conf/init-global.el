;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; スタートアップ時のメッセージを抑制
(setq inhibit-startup-message t)

;; エンコーディングは基本的にUTF-8
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-coding-system-priority 'utf-8)

;; メニューバー、ツールバー非表示
(eval-safe (menu-bar-mode 0))
(eval-safe (tool-bar-mode 0))

;; ヴィジブルベルを抑制
(setq visible-bell nil)

;; ビープ音を抑制
(setq ring-bell-function '(lambda ()))

;; カーソルの点滅を抑制
(blink-cursor-mode 0)

;; 行数、列数、時刻を表示
(line-number-mode t)
(column-number-mode t)
(display-time)

;; バックアップしない
(setq make-backup-files nil)

;; 自動保存しない
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)

;; yes/noを、y/nで選択できるようにする
(fset 'yes-or-no-p 'y-or-n-p)

;; kill-lineで行末の改行文字も削除
(setq kill-whole-line t)

;; リージョンをC-hで削除
(delete-selection-mode 1)

;; インデントはスペースで
(setq-default indent-tabs-mode nil)

;; バッファ末尾の余計な改行コードを防ぐ
(setq next-line-add-newlines nil)

;; 最終行に空行を追加する
(setq require-final-newline t)

;; The local variables list in .emacs と言われるのを抑止
(add-to-list 'ignored-local-variables 'syntax)

;; シンボリックリンク先がバージョン管理されていても問題ない
;; http://openlab.dino.co.jp/2008/10/30/212934368.html
(setq vc-follow-symlinks t)

;; C-j でインデント
(electric-indent-mode -1)
