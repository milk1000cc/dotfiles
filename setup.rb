#!/usr/bin/env ruby

# Thanks to: https://github.com/cho45/dotfiles

require 'pathname'
require 'fileutils'
require 'optparse'

include FileUtils::Verbose

def link(src, dst, platform = nil)
  puts "# #{ src } => #{ dst }"

  src = "platform/#{ platform }/#{ src }" if platform
  src = Pathname.new(src).expand_path

  dst = Pathname.new(dst).expand_path
  dst.parent.mkpath unless dst.parent.exist?
  remove_file dst if dst.symlink?
  remove_file dst if dst.file?

  ln_sf src.to_s, dst.to_s
  puts
end

def link_base_files
  link '.zshenv', '~/.zshenv'
  link '.zshrc', '~/.zshrc'

  link '.gemrc', '~/.gemrc'
  link '.railsrc', '~/.railsrc'

  link 'git/.gitconfig', '~/.gitconfig'
  link 'git/.gitignore', '~/.gitignore'

  link '.emacs.d', '~/.emacs.d'
end

def link_special_files(platform)
  return unless platform

  link '.tmux.conf', '~/.tmux.conf', platform
  link '.zshrc.local', '~/.zshrc.local', platform
  link '.bundle/config', '~/.bundle/config', platform
end

platform = nil

opt = OptionParser.new
opt.on('--platform=[PLATFORM]') { |v| platform = v }

opt.parse ARGV

link_base_files
link_special_files platform
