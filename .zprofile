OS="$(uname)"

if [[ "$OS" == "Darwin" ]]; then
    if [[ -d "/opt/homebrew" ]]; then
        eval $(/opt/homebrew/bin/brew shellenv)
    else
        eval $(/usr/local/bin/brew shellenv)
    fi
fi
