HOME=/home/lefant
SOURCE=$HOME/git/dotfiles-lefant

BUNDLE_LIB="$HOME/git/dotfiles-lefant/"

owner=lefant
group=lefant

mode=644


file $SOURCE/dot-Xresources $HOME/.Xresources
file $SOURCE/dot-bash_profile $HOME/.bash_profile
file $SOURCE/dot-emacs.el $HOME/.emacs.el
file $SOURCE/dot-erlang $HOME/.erlang
file $SOURCE/dot-perltidirc $HOME/.perltidirc
file $SOURCE/dot-plan $HOME/.plan
file $SOURCE/dot-ratpoisonrc $HOME/.ratpoisonrc
file $SOURCE/dot-screenrc $HOME/.screenrc
file $SOURCE/dot-signature $HOME/.signature
file $SOURCE/dot-xmodmap $HOME/.xmodmap
file $SOURCE/dot-zilerc $HOME/.zilerc
file $SOURCE/dot-zshrc $HOME/.zshrc
file $SOURCE/dot-dpkg.cfg $HOME/.dpkg.cfg
file $SOURCE/dot-gitconfig $HOME/.gitconfig
file $SOURCE/dot-screenrc.meta $HOME/.screenrc.meta
file $SOURCE/dot-debs $HOME/.debs
file $SOURCE/dot-debs-x $HOME/.debs-x
file $SOURCE/dot-xsession $HOME/.xsession

echo link $SOURCE $HOME

link $SOURCE/dot-aptitude $HOME/.aptitude
link $SOURCE/dot-bin $HOME/.bin
link $SOURCE/dot-darcs $HOME/.darcs
link $SOURCE/dot-devilspie $HOME/.devilspie
link $SOURCE/dot-mc $HOME/.mc
link $SOURCE/dot-templates $HOME/.templates
link $SOURCE/dot-elisp $HOME/.elisp


#filter $HOME/.zshrc '' 's/BOINGO-HOSTNAME/nixnix/'
