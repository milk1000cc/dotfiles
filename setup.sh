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

  command="ln -sf $src_fullpath $dest"
  echo $command
  $command

  echo
}

if ! [ -d $CONFIG_DIR ]; then
  mkdir $CONFIG_DIR
fi

link "emacs"
link "git"
link "tmux"
link ".zshenv" $HOME
link "zsh"

link ".bundle" $HOME
link ".gemrc" $HOME
link ".irbrc" $HOME
link ".default-gems" $HOME
link "rails"
