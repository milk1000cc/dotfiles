(defun add-to-load-path-recursively (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

(add-to-load-path-recursively "public_repos")

(require 'package)
(package-initialize)

(add-to-list 'load-path "~/.cask")
(require 'cask)
(cask-initialize)

(setq init-loader-show-log-after-init 'error-only)
(init-loader-load)
