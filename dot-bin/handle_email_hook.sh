#!/bin/sh

find /home/lefant/MaildirGandiLefant -mmin 20 -type f \
    -exec sh -c "cat {} |lbdb-fetchaddr -a" \;

mairix -F
