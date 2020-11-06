BREW_PREFIX=/usr/local/opt

path=(
    $BREW_PREFIX/node@14/bin
    $BREW_PREFIX/elasticsearch@6/bin
    $BREW_PREFIX/coreutils/libexec/gnubin
    $path
)

HISTFILE=~/Dropbox/zsh_history

alias diff='colordiff -ur'

export RUBY_CONFIGURE_OPTS="--with-openssl-dir=$BREW_PREFIX/openssl"

[[ -z $TMUX ]] && tmux
