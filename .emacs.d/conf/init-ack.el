;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; ack
;; 事前にackをcpanでインストールすること。debianパッケージだと、ack-grep
(defun ack ()
  (interactive)
  (let (
        (cmd "ack --nocolor --nogroup --smart-case ")
        )
    (setq cmd
          (read-string "run ack : " cmd))
    (compilation-start cmd 'grep-mode
                       `(lambda (name)
                          (format "*ack*<%s>" ,(current-time-string))))
    ))
