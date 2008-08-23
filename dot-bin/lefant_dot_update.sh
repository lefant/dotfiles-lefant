#!/usr/bin/zsh

pushd
cd ~/darcs/puppet-dotfiles
darcs pull -a
popd

puppet ~/darcs/puppet-dotfiles/lefant.pp
