#!/bin/sh

wget --recursive --convert-links --page-requisites $@

# --span-hosts
# --relative
# --no-parent
# --level=5
# --include-directories=a,b

# mmwiki hack
#find . -type f -exec sed -i "s/url('\/twiki/url('..\/..\/twiki/" {} \;
