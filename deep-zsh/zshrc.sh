#!/usr/bin/zsh

#disable shell suspend via CTRL-s
stty -ixon -ixoff

autoload -U promptinit
promptinit
setopt prompt_subst

#autoload -U checkmail            # needed for my prompt
autoload -U colors && colors     # make ${f,b}g{,_{,no_}bold} available
autoload -U edit-command-line    # later bound to C-z e or v in vi-cmd-mode
#autoload -U zed                  # what, your shell can't edit files? ["zed -f function"]
#autoload -U zmv                  # who needs mmv or rename? ["zmv '(*).lis' '\$1.txt"]

export WORDCHARS=''
bindkey -e                       # force keybindings no matter what $VISUAL and $EDITOR say

zmodload -i zsh/complist         # e.g. menu-select widget, list-colors [color specifications]
#zmodload -a zsh/stat stat
#zmodload -a zsh/zpty zpty
#zmodload -a zsh/zprof zprof
zmodload -ap zsh/mapfile mapfile

autoload -U compinit
compinit
setopt complete_in_word


HISTFILE=$HOME/.zsh/history

setopt hist_ignore_space hist_ignore_dups hist_reduce_blanks
setopt hist_verify appendhistory sharehistory
setopt EXTENDEDHISTORY
setopt extended_glob
HISTSIZE=100000
SAVEHIST=100000

REPORTTIME=10



export PAGER=less
export CVS_RSH=ssh

export PATH=$PATH:~/.bin


export TIMEFMT="%U user %S system %P cpu %*E total, running %J"
export COLORTERM=yes

[ -e /usr/bin/lessfile ] && eval $(lessfile)
[ -e /usr/bin/dircolors ] && eval $(dircolors)
export LESSCHARSET=UTF-8
export LESS='-iFRSX'

export GREP_OPTIONS="--color=auto --ignore-case --exclude-dir=.svn --exclude-dir=.git"

alias ls='ls --color=auto --classify'

alias greperl="grep --color=auto --ignore-case --exclude='.*' --include='*rl' --include='*inc' --exclude='.*' -r"

# someone said this helps with terminal weirdness
unset TERMCAP

export SCREEN_CAPTION_COLOR="`hostname |md5sum |head -c 1 |tr 0123456789abcdef YRGBCYMRGBCMYRGB`"


SSH_ASKPASS=`which ssh-askpass`
[[ $? = 0 ]] && export SSH_ASKPASS



if [ -d $HOME/.zsh ]
then
    for f in $HOME/.zsh/completion $HOME/.zsh/zshuser-${USER}.sh $HOME/.zsh/zshlocal.sh
    do
        [ -e $f ] && source $f
    done
fi
