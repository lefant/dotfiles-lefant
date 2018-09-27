. ~/.common.sh

export EMAIL="e@lefant.net"
#export DEBEMAIL="Fabian Linzberger <e@lefant.net>"
#export DARCS_EMAIL="Fabian Linzberger <e@lefant.net>"
#export REPORTBUGEMAIL="Fabian Linzberger <e@lefant.net>"
export GIT_AUTHOR_NAME="Fabian Linzberger"
export GIT_COMMITTER_NAME="Fabian Linzberger"
#export GIT_AUTHOR_EMAIL="e@lefant.net"
#export GIT_COMMITTER_EMAIL="e@lefant.net"
export GPGKEY=C02860F0

alias cpan_modulebuild_style_install="perl Build.PL; ./Build; ./Build test && sudo ./Build install"


[[ ! -e ~/.zsh/zshlocal.sh ]] && cp -v ~/.templates/zshlocal.sh ~/.zsh/zshlocal.sh
[[ -e ~/.emacs.d ]] && [[ ! -e ~/.emacs.d/local.el ]] && cp -v ~/.templates/local.el ~/.emacs.d/local.el

for dir in ~/.ssh/sock ~/.encfs
do
    [[ ! -d $dir ]] && mkdir -p $dir
done


#helper functions
find_best_editor () {
    if [ -e /tmp/emacs`id -u`/server -o -e /tmp/esrv`id -u`-`hostname` ]
    then
        export EDITOR="emacsclient -c -t"
    else
        export EDITOR=$(find_best mg zile nano vim)
    fi
    export VISUAL=$EDITOR
    alias e="$EDITOR"
}

fix_env () {
    find_best_editor

    watch=(notme)
    #setopt notify
}


# set screen / tmux window title to hostname / sudo command
cool_window_title() {
    local CMD=${1[(wr)^(*=*|sudo|ssh|-*)]}
    CMD=`echo $CMD|cut -f 1 -d "."`
    echo -ne "\ek$CMD\e\\"
    SCREENTITLE=$'%{\ekzsh\e\\%}'
}

chpwd() {
    export __CURRENT_GIT_BRANCH="$(parse_git_branch)"
}
PS1='%n@%m:%~/ $(echo $__CURRENT_GIT_BRANCH)$ '


# case dispatch on running local screen, meta screen or no screen at all (yet)
case $TMUX in
    *default*)
        preexec () {
            fix_env
        }
        precmd () {
            __CURRENT_GIT_BRANCH="$(parse_git_branch)"
        }
        setopt notify
        ;;
    *meta*)
        preexec () {
            cool_window_title $*
            fix_env
        }
        precmd () {
        }
        ;;
    *)
        fix_env

        if [ -z "$SSH_TTY" -a -z "$DOCKER_HACKBOX" ]; then
            case `hostname` in
                hecht)
                    ~/.bin/xplanet.sh &
                    setxkbmap dvorak -option compose:ralt -option compose:rwin -option ctrl:nocaps &
                    ;;
                *)
                    echo "unconfigured host: $(`hostname`)"
                    ;;
            esac

            unsetopt notify

            tmux -L meta -f ~/.tmux.conf.meta attach -t meta

            mount |grep -q ~/.encfs && fusermount -u -z ~/.encfs

            which kdestroy >/dev/null && kdestroy -q &>/dev/null

        else
            tmux -L default attach
        fi
        ;;
esac
