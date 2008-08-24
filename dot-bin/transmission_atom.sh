#!/bin/sh

TMPDIR=`mktemp -d`

case $1 in
    *.torrent)
        mv -v "$1" ${TMPDIR}/temp.torrent
        transmission-remote -x ssh atom.local transmission-proxy -a ${TMPDIR}/temp.torrent
        rm -rf ${TMPDIR}/temp.torrent
        ;;
    *)
        transmission-remote -x ssh atom.local transmission-proxy $@
        ;;
esac
