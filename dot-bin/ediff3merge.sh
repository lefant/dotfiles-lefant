#!/bin/bash -x

## downloaded from http://ulf.zeitform.de/de/dokumente/git-ediff

# tools
# tools
_EMACSCLIENT=emacsclient
_BASENAME=basename
_EGREP=egrep
_MKTEMP=mktemp

# args
_LOCAL=${1}
_REMOTE=${2}
if [ ${3} ] ; then
    _MERGED=${3}
else
    _MERGED=${_REMOTE}
fi
if [ ${4} -a -r ${4} ] ; then
    _BASE=${4}
    _EDIFF=ediff-merge-files-with-ancestor
    _EVAL="${_EDIFF} \"${_LOCAL}\" \"${_REMOTE}\" \"${_BASE}\" nil \"${_MERGED}\""
else
    _EDIFF=ediff-merge-files
    _EVAL="${_EDIFF} \"${_LOCAL}\" \"${_REMOTE}\" nil \"./${_MERGED}\""
fi

# run emacsclient
#${_EMACSCLIENT} -c -t -e "(${_EVAL})" 2>&1
emacs --eval "(${_EVAL})" 2>&1
