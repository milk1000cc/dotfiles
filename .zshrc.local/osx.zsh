BREW_PREFIX=/usr/local/opt

path=(
    $BREW_PREFIX/coreutils/libexec/gnubin
    $BREW_PREFIX/node@10/bin
    $path
)

HISTFILE=~/Dropbox/zsh_history

alias emacs='reattach-to-user-namespace emacs'
alias diff='colordiff -ur'

[[ -z $TMUX ]] && tmux
