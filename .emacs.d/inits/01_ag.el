(require 'ag)

;; 検索結果のハイライト
(setq ag-highlight-search t)

;; ファイルごとのグルーピングをしない
(setq ag-group-matches nil)

;; --hidden オプションを追加する (隠しファイルも検索対象)
(add-to-list 'ag-arguments "--hidden")

;; --stats オプションを除く
(setq ag-arguments (delete "--stats" ag-arguments))

;; .git ディレクトリを検索対象から除く
(setq-default ag-ignore-list '(".git"))
