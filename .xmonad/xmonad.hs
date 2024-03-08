{-# OPTIONS_GHC -Wno-deprecations #-}
import XMonad
import XMonad.Actions.CycleWS(nextWS, prevWS)
import XMonad.Hooks.DynamicLog(dynamicLogWithPP,ppOutput,ppTitle,xmobarPP,xmobarColor)
import XMonad.Hooks.EwmhDesktops(ewmh)
-- import XMonad.Hooks.ICCCMFocus(takeTopFocus)
import XMonad.Util.Run(spawnPipe,hPutStrLn)
import XMonad.Layout.NoBorders(smartBorders)
import XMonad.Hooks.ManageDocks(avoidStruts,ToggleStruts(..),docksStartupHook,docksEventHook)
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Layout.Fullscreen(fullscreenEventHook)
import XMonad.Util.Scratchpad(scratchpadSpawnActionTerminal,scratchpadManageHook)
import XMonad.ManageHook(doFloat,doShift,doIgnore)
-- import Xmonad.Hooks.ManageHelpers(doCenterFloat)
import qualified XMonad.Hooks.PerWindowKbdLayout as PWKbdLayout
import qualified XMonad.StackSet as W

myTerminal = "urxvt"

-- xFont = "-*-terminus-medium-r-normal-*-18-*-*-*-*-*-iso10646-*"
xFont = "Droid Sans Mono-13"
dmenuRun = "dmenu_run -fn \"" ++ xFont ++ "\" -nb '#000' -nf '#aaa' -sb '#aaa' -sf '#000'"

manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)
  where
    h = 0.8     -- terminal height, 40%
    w = 0.9     -- terminal width,  90%
    t = (1 - h) / 2     -- distance from top edge, 20%
    l = (1 - w) / 2    -- distance from left edge, 5%

myManageHook = composeAll
    [
      className =? "MPlayer"          --> doFloat
    -- , isFullscreen                    --> doFullFloat
    -- , className =? "Gimp"             --> doFloat
    , className =? "stalonetray"      --> doIgnore
    , className =? "trayer"           --> doIgnore
    , resource  =? "desktop_window"   --> doIgnore
    , resource  =? "kdesktop"         --> doIgnore
    -- , className =? "Firefox"          --> doShift "mail"
    -- , className =? "Chromium-browser" --> doShift "mail"
    -- , className =? "Chromium"         --> doShift "web"
    -- , className =? "Google-chrome"    --> doShift "web"
    , className =? "spotify"          --> doShift "spotify"
    , className =? "Cssh"             --> doFloat
    , className =? "nm-openconnect-auth-dialog" --> doShift "spotify"
    ]

extraWs = ["F" ++ (show i) | i <- [1..5]]

main = do
    xmproc <- spawnPipe "xmobar"
    -- autostart <- spawnPipe "sleep 5 && dex -a -s $HOME/.config/autostart"
    xmonad $ ewmh defaultConfig
        {
          modMask = mod4Mask
        , layoutHook = smartBorders $ avoidStruts $ layoutHook defaultConfig
        , terminal = myTerminal
        , manageHook = myManageHook <+> manageScratchPad <+> manageHook defaultConfig
        , logHook = dynamicLogWithPP xmobarPP
                       { ppOutput = hPutStrLn xmproc
                       , ppTitle = xmobarColor "#00dd00" "" -- . shorten 50
                       }
        , handleEventHook = handleEventHook defaultConfig
                            <+> PWKbdLayout.perWindowKbdLayout
                            <+> docksEventHook
                            <+> fullscreenEventHook
        , workspaces = map show [1..7] ++ ["pcmanfm", "spotify"] ++ extraWs
        , startupHook = startupHook defaultConfig <+> docksStartupHook
        } `additionalKeysP`
        ([
          ("M-b", sendMessage ToggleStruts)
        , ("M-p", spawn dmenuRun)
        , ("M-<Left>", prevWS)
        , ("M-<Right>", nextWS)
        , ("M-C-f", spawn "brightness -")
        , ("M-C-g", spawn "brightness +")
        , ("M-C-r", spawn "vol_ctl -")
        , ("M-C-t", spawn "vol_ctl +")
        , ("M-c", spawn "xcopy")
        , ("M-S-v", spawn "xpaste -p")
        , ("M-v", spawn "xpaste -b")
        , ("M-S-l", spawn "slock")
        -- scratchpad
        , ("M-`", scratchpadSpawnActionTerminal myTerminal)
        -- screenshot (full screen)
        , ("C-S-3", spawn "screenshot")
        -- screenshot (part of screen)
        , ("C-S-4", spawn "cropscreen")
        -- spotify
        , ("M-S-p", spawn "spotctl playpause")
        , ("M-S-n", spawn "spotctl next")
        ]
        ++ -- additional workspaces
        (concat [
          [ ( "M-<" ++ key ++ ">", windows $ W.greedyView key)
          , ( "M-S-<" ++ key ++ ">" , windows $ W.shift key)
          ] | key <- extraWs
        ]))
