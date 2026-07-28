# Thanks to:
# WEB+DB PRESS Vol.83 (https://gihyo.jp/magazine/wdpress/archive/2014/vol83)

HISTFILE=~/Dropbox/zsh_history
HISTSIZE=10000
SAVEHIST=10000

EMACS_COMMAND='env COLORTERM=1 emacs -nw'  # https://syohex.hatenablog.com/entry/2022/11/14/002626

export LANG='ja_JP.UTF-8'
export EDITOR=$EMACS_COMMAND
export LS_COLORS='di=01;36:ln=01;35:so=01;34:ex=01;31:bd=46;34:cd=43;34:su=41;30:sg=46;30'

bindkey '^r' history-incremental-pattern-search-backward  # glob (*) 検索ができるように

# /, -, ', " などで区切る
autoload -Uz select-word-style  # -U: alias 上書きを防ぐ, -z: zsh 形式 (https://medium.com/@rukurx/ad471efd84c3)
select-word-style default
zstyle ':zle:*' word-chars " /=;@:{},|-'\""
zstyle ':zle:*' word-style unspecified  # word-chars を区切り文字として扱う

# 補完
eval "$(brew shellenv)"
autoload -Uz compinit
compinit
zstyle ':completion:*' list-colors $LS_COLORS
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'  # 大文字・小文字を区別しない

setopt NO_FLOW_CONTROL  # ^Q/^S のフローコントロールを無効にする
setopt NO_BEEP
setopt SHARE_HISTORY
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_REDUCE_BLANKS
setopt LIST_PACKED  # ls などの補完を見やすく
setopt MAGIC_EQUAL_SUBST  # ./configure --prefix=xxx などの xxx を補完

alias ls='ls -F --color'
alias ll='ls -l'
alias g='git'
alias d='docker'
alias dc='docker compose'
alias r='rails'
alias e=$EMACS_COMMAND
alias vi='vim'
alias diff='colordiff -ur'
alias pg_dump='pg_dump -Fc --no-acl --no-owner'
alias pg_restore='pg_restore --clean --create --no-acl --no-owner -d postgres'

_update_curdir() {
  print -r -- $PWD >| ~/.curdir
}

autoload -Uz add-zsh-hook
add-zsh-hook chpwd _update_curdir

[[ -r ~/.curdir ]] && cd -- "$(< ~/.curdir)"

path=(
  /opt/homebrew/opt/coreutils/libexec/gnubin
  ~/.cargo/bin
  $path
)

[[ -z $TMUX ]] && tmux

eval "$(mise activate zsh)"
eval "$(starship init zsh)"
