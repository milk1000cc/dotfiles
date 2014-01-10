;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; git-root-grep
;; gitで管理しているディレクトリのルートから git grep する
;; http://d.hatena.ne.jp/authorNari/20091225/1261667956
(defun find-file-upward (name &optional dir)
  (setq dir (file-name-as-directory (or dir default-directory)))
  (cond
   ((string= dir (directory-file-name dir))
    nil)
   ((file-exists-p (expand-file-name name dir))
    (expand-file-name name dir))
   (t
    (find-file-upward name (expand-file-name ".." dir)))))
(defun git-root-grep ()
  (interactive)
  (let (
        (git-dir (concat (find-file-upward ".git") "/../"))
        (cmd "git --no-pager grep -n -i ")
        (origin-default-directory default-directory)
        )
    (setq default-directory git-dir)
    (setq cmd
          (read-string "run git root grep (like this) : " cmd))
    (compilation-start cmd 'grep-mode
                       `(lambda (name)
                          (format "*git-root-grep@%s*<%s>" ,git-dir,(current-time-string))))
    (setq default-directory origin-default-directory)))
