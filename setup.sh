#!/usr/bin/env sh

set -e

CONFIG_DIR="$HOME/.config"

expand_path() {
  local dir=$( cd $(dirname $1); pwd )
  local filename=$( basename $1 )

  echo "$dir/$filename"
}

link() {
  local src=$1
  local src_fullpath=$( expand_path $src )
  local dest=$2

  if [ -z $dest ]; then
    dest=$CONFIG_DIR
  fi

  echo "# $src => $dest"

  command="ln -sfn $src_fullpath $dest/$( basename $src )"
  echo $command
  $command

  echo
}

mkdir -p $CONFIG_DIR

link ".zshenv" $HOME
link "emacs"
link "git"
link "mise"
link "starship.toml"
link "tmux"
link "zsh"

mkdir -p "$HOME/.claude"
link "claude/settings.json" "$HOME/.claude"

link ".bundle" $HOME
link ".default-gems" $HOME
link ".gemrc" $HOME
link ".irbrc" $HOME
link "rails"
