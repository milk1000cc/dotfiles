;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
(eval-when-compile (require 'cl))

;; パッケージのインストール
;; http://light-of-moe.ddo.jp/~sakura/diary/?p=26
(defvar installing-package-list
  '(
    anything
    coffee-mode
    flymake
    grep-a-lot
    haml-mode
    js2-mode
    nginx-mode
    php-mode
    redo+
    rspec-mode
    sass-mode
    session
    slim-mode
    yaml-mode
    yasnippet
    inf-ruby)
)

(let ((not-installed (loop for x in installing-package-list
                            when (not (package-installed-p x))
                            collect x)))
  (when not-installed
    (package-refresh-contents)
    (dolist (pkg not-installed)
        (package-install pkg))))
