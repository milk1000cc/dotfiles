# Thanks to:
# WEB+DB PRESS Vol.83 (https://gihyo.jp/magazine/wdpress/archive/2014/vol83)
# https://qiita.com/mollifier/items/8d5a627d773758dd8078

HISTFILE=~/MyDrive/zsh_history
HISTSIZE=10000
SAVEHIST=10000

EMACS_COMMAND='env COLORTERM=1 emacs -nw'  # https://syohex.hatenablog.com/entry/2022/11/14/002626

export TERM='xterm-256color'
export LANG='ja_JP.UTF-8'
export EDITOR=$EMACS_COMMAND
export LS_COLORS='di=01;36:ln=01;35:so=01;34:ex=01;31:bd=46;34:cd=43;34:su=41;30:sg=46;30'
export DIRENV_LOG_FORMAT=""

bindkey '^r' history-incremental-pattern-search-backward  # glob (*) 検索ができるように

# /, -, ', " などで区切る
autoload -Uz select-word-style  # -U: alias 上書きを防ぐ, -z: zsh 形式 (https://medium.com/@rukurx/ad471efd84c3)
select-word-style default
zstyle ':zle:*' word-chars " /=;@:{},|-'\""
zstyle ':zle:*' word-style unspecified  # word-chars を区切り文字として扱う

# 補完
autoload -Uz compinit
zstyle ':completion:*' list-colors $LS_COLORS
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'  # 大文字・小文字を区別しない

# zmv
autoload -Uz zmv
alias zmv='noglob zmv -W'

# vcs_info
autoload -Uz vcs_info
zstyle ':vcs_info:git:*' formats '%b%m'
zstyle ':vcs_info:git+set-message:*' hooks git-stash-count

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
alias be='bundle exec'
alias diff='colordiff -ur'
alias pg_dump='pg_dump -Fc --no-acl --no-owner'
alias pg_restore='pg_restore --clean --create --no-acl --no-owner -d postgres'

+vi-git-stash-count() {
    local cnt

    cnt=$( git stash list 2>/dev/null | wc -l )

    if [[ $cnt -gt 0 ]]; then
        hook_com[misc]=":${cnt}"
    fi
}

_update_prompt() {
    local NEWLINE=$'\n'

    local -a messages1 messages2
    local ruby_version

    messages1+=( "%F{yellow}%B%~%b%f" )

    ruby_version=$( ruby -e "print RUBY_VERSION" )
    messages1+=( "%F{magenta}%B%U(${ruby_version})%u%b%f" )

    if [[ -n $vcs_info_msg_0_ ]]; then
        messages1+=( "%F{green}%B[${vcs_info_msg_0_}]%b%f" )
    fi

    messages2+=( "%F{blue}%B%n@%m$%b%f" )

    PROMPT="${(j: :)messages1}${NEWLINE}${(j: :)messages2} "
}

_update_curdir() {
    echo $PWD > $HOME/.curdir
}

autoload -Uz add-zsh-hook
add-zsh-hook precmd vcs_info
add-zsh-hook precmd _update_prompt
add-zsh-hook chpwd _update_curdir

init_homebrew() {
    if [[ -n $HOMEBREW_PREFIX ]]; then
        fpath=(
            $HOMEBREW_PREFIX/share/zsh/site-functions
            $fpath
        )

        compinit

        path=(
            $HOMEBREW_PREFIX/opt/coreutils/libexec/gnubin
            $path
        )

        source "$HOMEBREW_PREFIX/share/google-cloud-sdk/path.zsh.inc"
        source "$HOMEBREW_PREFIX/share/google-cloud-sdk/completion.zsh.inc"
    fi
}

init_homebrew

[[ -z $HOMEBREW_PREFIX ]] && compinit

eval "$(mise activate zsh)"

[[ -z $TMUX ]] && tmux

[[ -f "$HOME/.curdir" ]] && cd `cat $HOME/.curdir`
