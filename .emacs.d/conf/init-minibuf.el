;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; find-file 時に、大文字・小文字を区別しない
;; http://d.hatena.ne.jp/khiker/20061220/1166643421
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; ミニバッファのセッションを保存
;; http://d.hatena.ne.jp/higepon/20061230/1167447339
(setq session-initialize '(de-saveplace session keys menus)
      session-globals-include '((kill-ring 50)
                                (session-file-alist 500 t)
                                (file-name-history 10000)))
(setq session-globals-max-string 100000000)
(setq history-length t)
(add-hook 'after-init-hook 'session-initialize)

;; 同名ファイルが複数ある時に、わかりやすくする
;; http://clouder.jp/yoshiki/mt/archives/000673.html
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
