;; カーソル位置は保存しない
(setq session-initialize '(de-saveplace session))

(add-hook 'after-init-hook 'session-initialize)
