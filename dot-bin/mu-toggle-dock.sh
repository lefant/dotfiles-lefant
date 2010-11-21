#!/bin/sh

if [ $# -eq 1 ]
then
    case $1 in
        dock)
            xrandr --auto --output LVDS1 --off
            xrandr -s 0
            rm ~/.xmonad/xmonad.hs
            ln -s ~/.xmonad/xmonad-mu-diamond.hs ~/.xmonad/xmonad.hs
            xmonad --recompile
            xmonad --restart
            ;;
        launch)
            xrandr --auto --output VGA1 --off
            rm ~/.xmonad/xmonad.hs
            ln -s ~/.xmonad/xmonad-mu-ctrl-alt.hs ~/.xmonad/xmonad.hs
            xmonad --recompile
            xmonad --restart
            ;;
    esac
else
    echo "USAGE:"
    echo "$0 dock"
    echo "$0 launch"
fi
