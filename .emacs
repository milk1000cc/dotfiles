;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; eval-safe
;; 安全な評価。評価に失敗してもそこで止まらない
;; http://www.sodan.org/~knagano/emacs/dotemacs.html#eval-safe
(defmacro eval-safe (&rest body)
  `(condition-case err
       (progn ,@body)
     (error (message "[eval-safe] %s" err))))

(defun add-to-load-path (&rest paths)
  (mapc '(lambda (path)
           (add-to-list 'load-path path))
        (mapcar 'expand-file-name paths)))

(defvar run-linux (equal system-type 'gnu/linux))
(defvar run-darwin (equal system-type 'darwin))
(defvar run-carbon-emacs (and run-darwin window-system))
(defvar run-terminal-emacs (and run-darwin (not window-system)))

(add-to-load-path "~/.emacs.d/elisp"
                  "~/.emacs.d/conf"
                  "~/.cask")

(load "init-cask")
(load "init-color")
(load "init-session")
(load "init-global")
(load "init-keymaps")
(load "init-highlighting")
(load "init-minibuf")
(load "init-abbrves")
(load "init-dired")
(load "init-anything")
(load "init-redo")
(load "init-ack")
(load "init-git-root-grep")
(load "init-grep-edit")
(load "init-flymake")
(load "init-yasnippet")
(load "init-ruby")
(load "init-rails")
(load "init-javascript")
(load "init-web-mode")
(load "init-slim")
(load "init-coffee")
(load "init-rspec-mode")
(load "init-vue")

(when run-terminal-emacs
  (load "init-terminal-emacs")
  )
