#!/bin/sh

if [ $# -eq 0 ]
then
    dotfiledir="$HOME/git/lefant/dotfiles-lefant/"
else
    dotfiledir=$1
fi


# "condidional ln -s"
linkifnotexists () {
    source=$1
    dest=$2
    # remove argument if it is a dangling symlink
    if [ -L $dest -a ! -e $dest ]; then
        echo "DELETING dangling symlink $dest"
        rm $dest
    fi
    # test existance of 2nd argument, otherwise create link to 1st
    if [ -e $dest ]; then
        #echo -n "SKIPPING existing "
        ls -ld $dest |cut -d ' ' -f 8-10
    else
        ln -v -s $source $dest
    fi
}


find $dotfiledir -maxdepth 1 -name "dot-*" \
|while read sourcefile; do
    # translate leading 'dot-' into '~/.'
    destfile=~/.`basename $sourcefile|sed 's/dot-//g'`
    linkifnotexists $sourcefile $destfile
done


find $dotfiledir -maxdepth 1 -name "deep-*" \
|while read sourcedir; do
    # translate leading 'deep-' into '~/.'
    destdir=~/.`basename $sourcedir|sed 's/deep-//g'`
    [ -L $destdir -a ! -e $destdir ] && echo "DELETING dangling symlink $destdir" && rm $destdir
    [ -d $destdir ] || mkdir -v $destdir

    # now run find again for the files in this dir
    find $sourcedir -mindepth 1 \
    |while read sourcefile; do
        destfile=$destdir/`basename $sourcefile`
        linkifnotexists $sourcefile $destfile
    done
done

