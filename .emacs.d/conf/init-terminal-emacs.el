;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(gud-gdb-command-name "gdb --annotate=1")
 '(large-file-warning-threshold nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(highlight ((((class color) (min-colors 88) (background light)) (:background "darkseagreen2" :foreground "black"))))
 '(match ((((class color) (min-colors 88) (background light)) (:background "yellow" :foreground "black")))))

;; Mac Clipboard との共有
;; http://d.hatena.ne.jp/kiririmode/20110129/p1
(defvar prev-yanked-text nil "*previous yanked text")

(setq interprogram-cut-function
      (lambda (text &optional push)
        ; use pipe
        (let ((process-connection-type nil))
          (let ((proc (start-process "pbcopy" nil "pbcopy")))
            (process-send-string proc string)
            (process-send-eof proc)
            ))))

(setq interprogram-paste-function
      (lambda ()
        (let ((text (shell-command-to-string "pbpaste")))
          (if (string= prev-yanked-text text)
              nil
            (setq prev-yanked-text text)))))
