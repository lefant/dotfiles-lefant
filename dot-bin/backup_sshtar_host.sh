#!/bin/sh

host=${1}

ssh ${host} \
    "/bin/tar cvf - /bin/ /etc/ /lib/ /sbin/ /tmp/ /usr/" \
    | gzip \
    > ${host}_`date --iso`--`date +%s`.tar.gz
