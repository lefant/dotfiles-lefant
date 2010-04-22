import XMonad
import XMonad.Config.Gnome (gnomeConfig)
import XMonad.Config.Desktop (desktopLayoutModifiers)
import XMonad.Layout.NoBorders (smartBorders)

import System.Posix.Unistd (getSystemID, nodeName)


main = do
     host <- fmap nodeName getSystemID
     xmonad $ gnomeConfig {
         terminal = "rxvt-unicode"
       , workspaces = map show [1 .. 9]
       , layoutHook = smartBorders $ desktopLayoutModifiers layout
       , manageHook =
           composeAll
           [ manageHook gnomeConfig
           , className =? "MPlayer" --> doFloat
           , className  =? "URxvt" --> doShift "1"
           , className  =? "Thunderbird-bin" --> doShift "3"
           , className  =? "Icedove-bin" --> doShift "3"
           , className  =? "Firefox" --> doShift "4"
           , className  =? "Iceweasel" --> doShift "4"
           , className  =? "Google-chrome" --> doShift "4"
           , className  =? "Chromium-browser" --> doShift "4"
           , className  =? "Opera" --> doShift "4"
           , className  =? "Pidgin" --> doShift "5"
           , className  =? "xfce4-panel" --> doIgnore
           , resource  =? "desktop_window" --> doIgnore
           , resource  =? "kdesktop"       --> doIgnore
           ]

       -- modMask dependent on host
       , modMask =
           (if (host == "mu" || host == "end")
            then mod1Mask
            else mod4Mask)
       }

layout = Full ||| tiled ||| Mirror tiled
    where
     tiled = Tall nmaster delta ratio
     nmaster = 1
     ratio = 1/2
     delta = 3/100

    -- --
    -- -- MOD-{a,o,e}, Switch to physical/Xinerama screens 1, 2, or 3
    -- -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    -- --
    -- [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
    --     | (key, sc) <- zip [xK_a, xK_o, xK_e] [0..]
    --     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
