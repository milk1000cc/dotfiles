path=(
    /usr/local/opt/coreutils/libexec/gnubin
    $path
)

HISTFILE=~/Dropbox/zsh_history

alias diff='colordiff -ur'

[[ -z $TMUX ]] && tmux
