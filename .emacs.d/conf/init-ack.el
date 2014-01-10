;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; grepをackに変更
;; 事前にackをcpanでインストールすること。debianパッケージだと、ack-grep
(setq grep-command "ack --nocolor --nogroup --smart-case ")
(defun ack ()
  (interactive)
  (let ((grep-find-command "ack --nocolor --nogroup --smart-case "))
    (call-interactively 'grep-find)))
