BREW_PREFIX=/usr/local/opt

path=(
    $BREW_PREFIX/coreutils/libexec/gnubin
    $path
)

HISTFILE=~/Dropbox/zsh_history

alias diff='colordiff -ur'

export RUBY_CONFIGURE_OPTS="--with-openssl-dir=$BREW_PREFIX/openssl"

# google-cloud-sdk
source "/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.zsh.inc"
source "/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/completion.zsh.inc"

[[ -z $TMUX ]] && tmux
