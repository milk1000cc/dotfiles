;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; Mac 用設定
(setq default-input-method "MacOSX")
(setq mac-command-key-is-meta t)
(setq mac-command-modifier-meta t)
(setq mac-option-modifier 'meta)
(setq grep-find-use-xargs 'bsd)
(setq initial-frame-alist '((width . 110) (height . 35) (top . 130) (left . 450)))

;; 半透明化パッチ適用
(setq default-frame-alist
      (append (list '(alpha . (90 90))) default-frame-alist))

;; バッテリ残量表示
(eval-safe (display-battery-mode t))

;; バックスラッシュを入力
;; http://lists.sourceforge.jp/mailman/archives/macemacsjp-users/2006-June/001125.html
(define-key global-map [2213] nil)
(define-key global-map [67111077] nil)
(define-key global-map [134219941] nil)
(define-key global-map [201328805] nil)
(define-key function-key-map [2213] [?\\])
(define-key function-key-map [67111077] [?\C-\\])
(define-key function-key-map [134219941] [?\M-\\])
(define-key function-key-map [201328805] [?\C-\M-\\])
(define-key global-map [3420] nil)
(define-key global-map [67112284] nil)
(define-key global-map [134221148] nil)
(define-key global-map [201330012] nil)
(define-key function-key-map [3420] [?\\])
(define-key function-key-map [67112284] [?\C-\\])
(define-key function-key-map [134221148] [?\M-\\])
(define-key function-key-map [201330012] [?\C-\M-\\])

;; Command-Key and Option-Key
;; http://journal.mycom.co.jp/column/osx/332/index.html
(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))

;; hide scroll bar
(set-scroll-bar-mode nil)

;; http://sakito.jp/emacs/emacs23.html#id15
(create-fontset-from-ascii-font "Menlo-14:weight=normal:slant=normal" nil "menlokakugo")
(set-fontset-font "fontset-menlokakugo"
                  'unicode
                  (font-spec :family "Hiragino Kaku Gothic ProN" :size 12)
                  nil
                  'append)
(add-to-list 'default-frame-alist '(font . "fontset-menlokakugo"))
