{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

import XMonad
--import XMonad.Config.Gnome (gnomeConfig, gnomeRun)
import XMonad.Config.Desktop (desktopLayoutModifiers)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Hooks.EwmhDesktops (ewmhDesktopsStartup)
import XMonad.Layout.Spiral (spiral)

import qualified XMonad.StackSet as W
import Data.Ratio
import Control.Monad


-- import XMonad.Hooks.ManageDocks (ToggleStruts(..))
-- import XMonad.Util.EZConfig

import System.Posix.Unistd (getSystemID, nodeName)


main = do
     host <- fmap nodeName getSystemID
     let myConfig = defaultConfig {
                           terminal = "rxvt-unicode"
                         , workspaces = map show [1 .. 9]
                         , layoutHook = smartBorders $ desktopLayoutModifiers layout
                         , startupHook = ewmhDesktopsStartup >> setWMName "LG3D"
                         , manageHook =
                             composeAll
                             [ manageHook myConfig
                             , className =? "MPlayer" --> doFloat
                             , className  =? "URxvt" --> doShift "1"
                             , className  =? "Thunderbird-bin" --> doShift "3"
                             , className  =? "Icedove-bin" --> doShift "3"
                             , className  =? "Firefox" --> doShift "3"
                             , className  =? "Iceweasel" --> doShift "3"
                             , className  =? "Google-chrome" --> doShift "4"
                             , className  =? "Chromium-browser" --> doShift "4"
                             , className  =? "Opera" --> doShift "3"
                             , className  =? "Pidgin" --> doShift "5"
                             -- stop mnesia:tv from resizing like crazy
                             , className  =? "Toplevel" --> doFloat
                             , className  =? "xfce4-panel" --> doIgnore
                             , resource  =? "desktop_window" --> doIgnore
                             , resource  =? "kdesktop"       --> doIgnore
                             ]

                         -- modMask dependent on host
                         , modMask =
                               case host of
                                 "end" -> mod1Mask .|. controlMask
                                 -- "mu" -> mod1Mask .|. controlMask
                                 _Other -> mod4Mask
                         }

     xmonad myConfig

     -- let myGnomeConfig' = removeKeysP myGnomeConfig
     --                      [ "M-b"
     --                      , "M-p"
     --                      , "M-q"
     --                      , "M-w"
     --                      , "M-<Space>"
     --                      ]
     -- let myGnomeConfig'' = additionalKeysP myGnomeConfig'
     --                       [ ("M-C-q",
     --                          broadcastMessage ReleaseResources >> restart "xmonad" True)
     --                       , ("M-C-p", gnomeRun)
     --                       , ("M-C-b", sendMessage ToggleStruts)
     --                       ]
     -- xmonad myGnomeConfig''




-- layout = tiled ||| Full ||| spiral (6/7) ||| ThreeKnut 1 (1/3) (3/100) (1/2) ||| Mirror tiled
layout = tiled ||| Full ||| spiral (6/7) ||| Mirror tiled
    where
     tiled = Tall nmaster delta ratio
     nmaster = 1
     ratio = 1/2
     delta = 3/100

-- layout = Full ||| tiled ||| Mirror tiled
--     where
--      tiled = Tall nmaster delta ratio
--      nmaster = 1
--      ratio = 1/2
--      delta = 3/100

    -- --
    -- -- MOD-{a,o,e}, Switch to physical/Xinerama screens 1, 2, or 3
    -- -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    -- --
    -- [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
    --     | (key, sc) <- zip [xK_a, xK_o, xK_e] [0..]
    --     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
