(require 'js2-mode)

(setq js2-basic-offset 2)
(setq js2-include-node-externs t)
(setq js2-strict-missing-semi-warning nil)
(setq js2-global-externs '("location" "gon"))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
