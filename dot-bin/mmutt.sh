#!/bin/sh

mairix $@
mutt -R -f ~/.mairix-results/
