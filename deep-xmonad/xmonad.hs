{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

import XMonad
import XMonad.Config.Gnome (gnomeConfig, gnomeRun)
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
     let myGnomeConfig = gnomeConfig {
                           terminal = "rxvt-unicode"
                         , workspaces = map show [1 .. 9]
                         , layoutHook = smartBorders $ desktopLayoutModifiers layout
                         , startupHook = ewmhDesktopsStartup >> setWMName "LG3D"
                         , manageHook =
                             composeAll
                             [ manageHook gnomeConfig
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
                             (if (host == "mu" || host == "end")
                              then mod1Mask .|. controlMask
                              else mod4Mask)
                         }

     xmonad myGnomeConfig

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




layout = tiled ||| Full ||| spiral (6/7) ||| ThreeKnut 1 (1/3) (3/100) (1/2) ||| Mirror tiled
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



-- | Arguments are nmaster, delta, fraction
data ThreeKnut a = ThreeKnut { threeKnutNMaster    :: !Int
                             , threeKnutFirst      :: !Rational
                             , threeKnutDelta      :: !Rational
                             , threeKnutFrac       :: !Rational
                             }
    deriving (Show,Read)

instance LayoutClass ThreeKnut a where
    pureLayout (ThreeKnut e n _ f) r    = doL e n f r
    handleMessage l m =
        return $ msum [fmap resize     (fromMessage m)
                      ,fmap incmastern (fromMessage m)]
            where resize Shrink = l { threeKnutFrac = max (-0.5) $ f-d }
                  resize Expand = l { threeKnutFrac = min 1 $ f+d }
                  incmastern (IncMasterN x) = l { threeKnutNMaster = max 0 (n+x) }
                  n = threeKnutNMaster l
                  d = threeKnutDelta l
                  f = threeKnutFrac l
    description _ = "ThreeKnut"

doL :: Int -> Rational -> Rational -> Rectangle -> W.Stack a -> [(a, Rectangle)]
doL n e f r = ap zip (tile3 e f r n . length) . W.integrate

-- | tile3.  Compute window positions using 3 panes
tile3 :: Rational -> Rational -> Rectangle -> Int -> Int -> [Rectangle]
tile3 e f r nmaster n
    | n <= nmaster || nmaster == 0 = splitVertically n r
    | n <= nmaster+1 = splitVertically nmaster s1 ++ splitVertically (n-nmaster) s2
    | otherwise = splitVertically nmaster r1 ++ splitVertically nslave1 r2 ++ splitVertically nslave2 r3
        where (r1, r2, r3) = split3HorizontallyBy e f r
              (s1, s2) = splitHorizontallyBy f r
              nslave = (n - nmaster)
              -- nslave1 = ceiling (nslave % 2)
              nslave1 = 1
              nslave2 = (n - nmaster - nslave1)

split3HorizontallyBy :: Rational -> Rational -> Rectangle -> (Rectangle, Rectangle, Rectangle)
split3HorizontallyBy e f (Rectangle sx sy sw sh) =
    ( Rectangle sx sy r1w sh
    , Rectangle (sx + fromIntegral r1w) sy r2w sh
    , Rectangle (sx + fromIntegral r1w + fromIntegral r2w) sy r3w sh )
    where
      r1w = ceiling $ fromIntegral sw * e
      -- r2w = ceiling ( (sw - r1w) % 2 )
      r2w = ceiling $ fromIntegral (sw - r1w) * f
      r3w = sw - r1w - r2w



