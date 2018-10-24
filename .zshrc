stty stop undef

path=(
    $HOME/local/bin
    $HOME/.rbenv/bin
    $HOME/.yarn/bin
    $HOME/.cask/bin

    /usr/local/sbin
    /usr/local/bin

    /usr/bin
    /bin
    /usr/sbin
    /sbin
)

export EDITOR="emacs -nw"
export LANG=ja_JP.UTF-8
export LSCOLORS=GxFxExdxBxegedabagacad
export LS_COLORS='di=01;36:ln=01;35:so=01;34:ex=01;31:bd=46;34:cd=43;34:su=41;30:sg=46;30'
export WORDCHARS='*?[]~=&;!#$%^(){}<>'

bindkey -e
bindkey '^R' history-incremental-pattern-search-backward
bindkey '^S' history-incremental-pattern-search-forward

## modules
autoload -U compinit
compinit -u
zstyle ':completion:*' list-colors 'di=;36;1' 'ln=;35;1' 'so=;34;1' 'ex=31;1' 'bd=46;34' 'cd=43;34'
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

autoload -Uz url-quote-magic
zle -N self-insert url-quote-magic

autoload -Uz zmv

## setopt
setopt hist_ignore_dups
setopt share_history
setopt auto_cd
setopt auto_pushd
setopt list_packed
setopt no_beep
setopt magic_equal_subst

HISTFILE=~/.zsh_history
HISTSIZE=9999999
SAVEHIST=9999999

MY_NAME=`hostname -f`

alias ls="ls -F --color"
alias ll="ls -l"
alias g="git"
alias emacs="emacs -nw"
alias r="rails"
alias e="emacs"
alias vi="vim"
alias be="bundle exec"
alias zmv="noglob zmv -W"

function set-git-current-branch-env() {
    GIT_CURRENT_BRANCH=$( git symbolic-ref --short HEAD 2> /dev/null )
}

function set-git-stash-env() {
    # Thanks to: http://qiita.com/Cside/items/13f85c11d3d0aa35d7ef
    local cnt=$( git stash list 2>/dev/null | wc -l | tr -d ' ' )
    if [ "$cnt" -gt 0 ]; then
      GIT_STASH=":$cnt"
    else
      GIT_STASH=""
    fi
}

function set-current-rbenv-env() {
    if which rbenv > /dev/null; then
      CURRENT_RUBY=$( rbenv version | sed -e 's/ .*//' )
    fi
}

function update-prompt() {
    if [ -n "${CURRENT_RUBY}" ]; then
        RUBY_PROMPT_STRING="%{[01;04;35m%}(${CURRENT_RUBY})%{[m%} "
    else
        RUBY_PROMPT_STRING=""
    fi

    if [ -n "${GIT_CURRENT_BRANCH}" ]; then
        set-git-stash-env
        GIT_PROMPT_STRING="%{[01;32m%}[$GIT_CURRENT_BRANCH$GIT_STASH]%{[m%}"
    else
        GIT_PROMPT_STRING=""
    fi

    if [ -z "${REMOTEHOST}${SSH_CONNECTION}" ] || [ "`echo $SSH_CONNECTION | cut -d ' ' -f 3 | grep '^192\.168'`" ]; then
        REMOTE_PROMPT_STRING=""
        NAME_PROMPT_STRING="%{[01;34m%}${LOGNAME}@${MY_NAME}$%{[m%}"
    else
        REMOTE_PROMPT_STRING="%{[01;31m%}REMOTE%{[m%} "
        NAME_PROMPT_STRING="%{[01;04;31m%}${LOGNAME}@${MY_NAME}$%{[m%}"
    fi

    PROMPT="${REMOTE_PROMPT_STRING}%{[m%}%{[01;33m%}%~ %{[m%}${RUBY_PROMPT_STRING}${GIT_PROMPT_STRING}
${NAME_PROMPT_STRING} "
}

function precmd() {
    set-git-current-branch-env
    set-current-rbenv-env
    update-prompt
}

function cd() {
    builtin cd "$@"
    echo $PWD > $HOME/.curdir
}

if [ -f "$HOME/.zshrc.mine" ]; then
    source "$HOME/.zshrc.mine"
fi

if which rbenv > /dev/null; then
    eval "$(rbenv init -)"
fi

export PATH=./bin:$PATH

if [ -f "$HOME/.curdir" ]; then
    cd `cat $HOME/.curdir`
fi
