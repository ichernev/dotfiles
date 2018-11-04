import XMonad
import XMonad.Actions.CycleWS(nextWS, prevWS)
import XMonad.Hooks.DynamicLog(dynamicLogWithPP,ppOutput,ppTitle,xmobarPP,xmobarColor)
import XMonad.Hooks.EwmhDesktops(ewmh)
import XMonad.Hooks.ICCCMFocus(takeTopFocus)
import XMonad.Util.Run(spawnPipe,hPutStrLn)
import XMonad.Layout.NoBorders(smartBorders)
import XMonad.Hooks.ManageDocks(avoidStruts,ToggleStruts(..),docksStartupHook,docksEventHook)
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Layout.Fullscreen(fullscreenEventHook)
import XMonad.Util.Scratchpad(scratchpadSpawnActionTerminal,scratchpadManageHook)
import qualified XMonad.Hooks.PerWindowKbdLayout as PWKbdLayout
import qualified XMonad.StackSet as W

myTerminal = "urxvt"

-- xFont = "-*-terminus-medium-r-normal-*-18-*-*-*-*-*-iso10646-*"
xFont = "Droid Sans Mono-12"
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
    , className =? "Gimp"             --> doFloat
    , className =? "stalonetray"      --> doIgnore
    , className =? "trayer"           --> doIgnore
    , resource  =? "desktop_window"   --> doIgnore
    , resource  =? "kdesktop"         --> doIgnore
    -- , className =? "Firefox"          --> doShift "mail"
    -- , className =? "Chromium-browser" --> doShift "mail"
    -- , className =? "Chromium"         --> doShift "web"
    -- , className =? "Google-chrome"    --> doShift "web"
    -- , className =? "Spotify"          --> doShift "spotify"
    , className =? "Cssh"             --> doFloat
    ]

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ ewmh defaultConfig
        {
          modMask = mod4Mask
        , layoutHook = smartBorders $ avoidStruts $ layoutHook defaultConfig
        , terminal = myTerminal
        , manageHook = myManageHook <+> manageScratchPad <+> manageHook defaultConfig
        , logHook = takeTopFocus
                    <+> dynamicLogWithPP xmobarPP
                       { ppOutput = hPutStrLn xmproc
                       , ppTitle = xmobarColor "green" "" -- . shorten 50
                       }
        , handleEventHook = handleEventHook defaultConfig
                            <+> PWKbdLayout.perWindowKbdLayout
                            <+> docksEventHook
                            <+> fullscreenEventHook
        , startupHook = startupHook defaultConfig <+> docksStartupHook
        } `additionalKeysP`
        [
          ("M-b", sendMessage ToggleStruts)
        , ("M-p", spawn dmenuRun)
        , ("M-<Left>", prevWS)
        , ("M-<Right>", nextWS)
        , ("M-C-f", spawn "brightness -")
        , ("M-C-g", spawn "brightness +")
        , ("M-C-r", spawn "vol_ctl -")
        , ("M-C-t", spawn "vol_ctl +")
        , ("M-c", spawn "xcopy")
        , ("M-v", spawn "xselectcb")
        , ("M-S-l", spawn "slock")
        -- scratchpad
        , ("M-`", scratchpadSpawnActionTerminal myTerminal)
        -- screenshot (full screen)
        , ("M-S-s", spawn "screenshot")
        -- screenshot (part of screen)
        , ("M-S-p", spawn "cropscreen")
        ]
