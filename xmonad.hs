{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

-- Xmonad
import XMonad

-- Hooks
import XMonad.Hooks.ManageHelpers ( doCenterFloat, isDialog )
import XMonad.Hooks.EwmhDesktops ( ewmh, ewmhFullscreen )
import XMonad.Hooks.DynamicLog

-- Layout
import XMonad.Layout.Accordion ( Accordion(Accordion) )
import XMonad.Layout.Magnifier ( magnifiercz )
import XMonad.Layout.Renamed ( renamed, Rename(Replace, CutWordsLeft) )
import XMonad.Layout.Spacing ( spacingRaw, Border(Border) )
import XMonad.Layout.ResizableTile ( MirrorResize(MirrorExpand, MirrorShrink), ResizableTall(ResizableTall) )

-- Running things
import XMonad.Util.SpawnOnce ( spawnOnce )
import XMonad.Util.EZConfig ( additionalKeysP )

-- Modifying windows
import XMonad.Actions.RotSlaves ( rotAllDown, rotAllUp )
import XMonad.Actions.WithAll ( killAll )

-- Other imports
import Keybinds ( categoryTextFormat, unwrapCategories, KeyMapKey(..), KeyMapCategory(MkCat) )
import Control.Monad ((>=>))
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import XMonad.Hooks.StatusBar (withEasySB, statusBarProp, StatusBarConfig)

myTerminal      = "kitty"
myFocusFollowsMouse = True
myClickJustFocuses = False
myBorderWidth   = 3
myModMask       = mod4Mask
myWorkspaces    = map show [1 .. 9]
myNormalBorderColor  = "#10000a"
myFocusedBorderColor = "#ddf0ff"

------------------------------------------------------------------------
-- Key and mouse bindings:
myKeyMap :: [ KeyMapCategory ]
myKeyMap =
    [ MkCat "App launching"
      [ MkKey "M-S-<Return>"
          (spawn myTerminal)
          "Launch a terminal"
      , MkKey "M-C-p"
          (spawn "rofi -show scripts")
          "Launch rofi in script mode"
      , MkKey "M-p"
          (spawn "rofi -show drun")
          "Launch rofi"
      , MkKey "M-="
          (spawn "rofi -show calc -modi calc -no-show-match -no-sort -calc-command \"echo -n '{result}' | xclip\"")
          "Launch rofi in calculator mode"
      , MkKey "C-S-v"
          (spawn "xfce4-popup-clipman")
          "Launch clipman history chooser"
      , MkKey "M-e"
          (spawn $ myTerminal ++ " -e mc")
          "Launch file manager"
      , MkKey "M-S-e"
          (spawn "nautilus")
          "Launch graphical file manager"
      ]
    , MkCat "Windows"
      [ MkKey "M-S-c"
          kill
          "Close focused window"
      , MkKey "M-C-c"
          killAll
          "Close all windows on active screen"
      , MkKey "M-n"
          refresh
          "Resize viewed windows to the correct size"
      , MkKey "M-<Tab>"
          (windows W.focusDown)
          "Move focus to the next window"
      , MkKey "M-S-<Tab>"
          (windows W.focusUp)
          "Move focus to the previous window"
      , MkKey "M-C-<Tab>"
          rotAllUp
          "Rotate all windows up"
      , MkKey "M-S-C-<Tab>"
          rotAllDown
          "Rotate all windows down"
      , MkKey "M-m"
          (windows W.focusMaster)
          "Move focus to the master window"
      , MkKey "M-<Return>"
          (windows W.swapMaster)
          "Swap the focused window and the master window"
      , MkKey "M-t"
          (withFocused $ windows . W.sink)
          "Push window back into tiling"
      ]
    , MkCat "Workspaces & Screens"
        [ MkRangeKey myWorkspaces myWorkspaces myWorkspaces ("M-" ++) ("Swich to workspace " ++) (windows . W.greedyView)
        , MkRangeKey myWorkspaces myWorkspaces myWorkspaces ("M-S-" ++) ("Move focused window to workspace " ++) (windows . W.shift)
        , MkRangeKey [ "j", "k", "l" ] [ 0 .. ] [ 1 .. ] ("M-" ++) ("Swich to screen " ++) (screenWorkspace >=> flip whenJust (windows . W.view))
        , MkRangeKey [ "j", "k", "l" ] [ 0 .. ] [ 1 .. ] ("M-S-" ++) ("move focused window to screen " ++) (screenWorkspace >=> flip whenJust (windows . W.shift))
        ]
    , MkCat "Layouts"
      [ MkKey "M-<Space>"
          (sendMessage NextLayout)
          "Move to next layout"
      , MkKey "M-S-<Space>"
          (sendMessage FirstLayout)
          "Return to the first layout"
      , MkKey "M-."
          (sendMessage Expand)
          "Expand the master area"
      , MkKey "M-S-."
          (sendMessage Shrink)
          "Shrink the master area"
      , MkKey "M-C-."
          (sendMessage MirrorShrink)
          "Expand the master are vertically"
      , MkKey "M-C-S-."
          (sendMessage MirrorExpand)
          "Shrink the master area vertically"
      , MkKey "M-,"
          (sendMessage (IncMasterN 1))
          "Increment the number of windows in master area"
      , MkKey "M-S-,"
          (sendMessage (IncMasterN (-1)))
          "Decrement the number of windows in master area"
      ]
    , MkCat "Utility"
        [ MkKey "<Print>"
          (spawn "maim -s | xclip -selection clipboard -t image/png")
          "Print screen allowing user to select an area"
        , MkKey "S-<Print>"
            (spawn "maim -i $(xdotool getactivewindow) | xclip -selection clipboard -t image/png")
            "Print active window"
        , MkKey "M-z"
            (spawn "boomer")
            "Zoom the desktop"
        , MkKey "M-S-/"
            (spawn $ unwords
                [ "cat <<EOF |"
                , "rofi"
                , "-p Bindings"
                , "-columns 1"
                , "rofi -theme-str 'window {width: 1000px; height: 1000px;}'"
                , "-dmenu\n" ++ categoryTextFormat myKeyMap ++ "\nEOF"
                ])
            "Show the key bindings"
        ]
    , MkCat "XMonad"
      [ MkKey "M-S-p"
          (spawn "rofi -l 5 -modi \"power:~/.scripts/rofi/power-menu.sh\" -show power")
          "Power menu"
      , MkKey "M-q"
          (spawn "xmonad --recompile; xmonad --restart")
          "Restart xmonad"
      ]
    ]


myMouseBindings XConfig {XMonad.modMask = modm} = M.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:
myLayout = spacing ( tiled ||| full ||| tiledMag ||| accordion )
  where
     -- Additional spacing
     spacing = renamed [ CutWordsLeft 1 ]
             . spacingRaw True (Border 0 5 5 5) True (Border 5 5 5 5) True

     -- Tiling algorithms
     tiled     = withName "Mstk" $ ResizableTall master delta ratio []
     full      = withName "Full" Full
     tiledMag  = withName "MMst" $ magnifiercz 1.25 tiled
     accordion = withName "Acor" $ Mirror Accordion

     -- Renaming
     withName newName = renamed [ Replace newName ]

     -- Tiling parameters
     master = 1
     ratio  = 1/2
     delta  = 3/100

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ isDialog                              --> doCenterFloat
    , stringProperty "WM_NAME" =? "Vimaze"  --> doFloat
    , className =? "obs"                    --> doFloat
    , className =? "battle.net.exe"         --> doFloat
    , className =? "Yad"                    --> doCenterFloat
    , className =? "Xfce4-clipman-settings" --> doCenterFloat
    , className =? "Xfce4-clipman-history"  --> doFloat
    , className =? "Synergy"                --> doFloat
    , resource  =? "desktop_window"         --> doIgnore
    , resource  =? "kdesktop"               --> doIgnore ]

------------------------------------------------------------------------
-- Event handling

-- Nothing for now.


------------------------------------------------------------------------
-- Status bars and logging
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = return ()

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
    spawnOnce "lxsession &"
    spawnOnce "picom --experimental-backends &"
    spawnOnce "~/.scripts/randomwall.sh &"
    spawnOnce "xscreensaver -no-splash &"
    spawnOnce $ unwords
        [ "trayer"
        , "--monitor primary"
        , "--edge top --align right"
        , "--SetDockType true"
        , "--SetPartialStrut true"
        , "--expand false"
        , "--widthtype request"
        , "--margin 0"
        , "--padding 5"
        , "--transparent true"
        , "--alpha 10"
        , "--iconspacing 2"
        , "--tint 0x000000"
        , "--heighttype pixel"
        , "--height 21 &"
        ]
    spawnOnce "dunst &"
    spawnOnce "blueman-applet &"
    spawnOnce "nm-applet --sm-disable &"
    spawnOnce "xfce4-clipman &"
    spawnOnce "dropbox &"
    spawnOnce "keepassxc &"
    --spawnOnce "xbindkeys"


------------------------------------------------------------------------
-- Custom pretty printer
myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = blue " • "
    , ppTitle           = wrap (white "[ ") (white " ]") . green . ppWindow
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap (blue "[") (blue "]")
    , ppVisible         = wrap (lowWhite "[") (lowWhite "]")
    , ppHidden          = white
    , ppHiddenNoWindows = darkGray
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    }
  where
    ppWindow = xmobarRaw . (\w -> if null w then "Untitled" else w) . shorten 30
    blue     = xmobarColor "#6A91E7" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""
    green    = xmobarColor "#309f3c" ""
    darkGray = xmobarColor "#4E4E4E" ""


main :: IO ()
main = xmonad
       . ewmhFullscreen
       . ewmh
       . withEasySB sbConfig toggleStrutsKey 
       $ myConfig
  where
    sbConfig :: StatusBarConfig
    sbConfig = statusBarProp "LANG=en_US.UTF-8 xmobar" (pure myXmobarPP)

    toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
    toggleStrutsKey XConfig { modMask = m } = (m, xK_b)

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
myConfig = def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = const M.empty,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    } `additionalKeysP` unwrapCategories myKeyMap
