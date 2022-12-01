#!/usr/bin/env ruby

# Thanks to: https://github.com/cho45/dotfiles

require 'pathname'
require 'fileutils'

include FileUtils::Verbose

def link(src, dst = (ENV['XDG_CONFIG_HOME'] || '~/.config'))
  puts "# #{ src } => #{ dst }"

  src = Pathname.new(src).expand_path

  dst = Pathname.new(dst).expand_path
  dst.parent.mkpath unless dst.parent.exist?
  remove_file dst if dst.symlink?
  remove_file dst if dst.file?

  ln_sf src.to_s, dst.to_s
  puts
end

link '.zshenv', '~/.zshenv'
link 'zsh'

link '.irbrc', '~/.irbrc'
link '.gemrc', '~/.gemrc'
link 'rails'
link ".bundle/config", '~/.bundle/config'

link 'git'
link 'emacs'
link 'tmux'
