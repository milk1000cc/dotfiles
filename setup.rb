#!/usr/bin/env ruby

# Thanks to: https://github.com/cho45/dotfiles

require 'pathname'
require 'fileutils'

include FileUtils::Verbose

def link(src, dst)
  puts "# #{ src } => #{ dst }"

  src = Pathname.new(src).expand_path

  dst = Pathname.new(dst).expand_path
  dst.parent.mkpath unless dst.parent.exist?
  remove_file dst if dst.symlink?
  remove_file dst if dst.file?

  ln_sf src.to_s, dst.to_s
  puts
end

link '.zprofile', '~/.zprofile'
link '.zshrc', '~/.zshrc'

link '.gemrc', '~/.gemrc'
link '.railsrc', '~/.railsrc'
link ".bundle/config", '~/.bundle/config'

link 'git/.gitconfig', '~/.gitconfig'
link 'git/.gitignore', '~/.gitignore'

link '.emacs.d', '~/.emacs.d'

link '.tmux.conf', '~/.tmux.conf'
