#!/bin/sh

targethost=$1

cat <<EOF |ssh -t $targethost
if [ ! -e /users/fabian/git/lefant/dotfiles-lefant ]
then
    mkdir -p git/lefant
    cd git/lefant
    git clone git://github.com/lefant/dotfiles-lefant.git
    cd
    sh git/lefant/dotfiles-lefant/dot-bin/linkifnotexists.sh
    cd .zsh
    ln -s zshuser-lefant.sh zshuser-fabian.sh
    cd
fi
EOF

ssh $targethost
