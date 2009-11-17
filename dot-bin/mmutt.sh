#!/bin/sh

mairix $@
mutt -f ~/.mairix-results/
