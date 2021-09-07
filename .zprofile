OS="$(uname)"
UNAME_MACHINE="$(/usr/bin/uname -m)"

if [[ "$OS" == "Darwin" ]]; then
    if [[ "$UNAME_MACHINE" == "arm64" ]]; then
        eval $(/opt/homebrew/bin/brew shellenv)
    else
        eval $(/usr/local/bin/brew shellenv)
    fi
fi
