-- ~/.xmonad/xmonad.hs

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.Place
-- import XMonad.Hooks.SetWMName

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import XMonad.Util.Scratchpad

import System.IO

import XMonad.Actions.SpawnOn
import XMonad.Actions.GridSelect

import Data.Ratio

import XMonad.Layout
import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns
import XMonad.Layout.DecorationMadness
import XMonad.Layout.CenteredMaster

import XMonad.StackSet as W

import Solarized

--import XMonad.Actions.Volume

myTerminal = "terminator"
myWorkspaces = [ "1:email"
               , "2:terminals"
               , "3:db"
               , "4:kettle"
               , "5:ide"
               , "6:vbox"
               , "7:folders"
               , "8:misc"
               , "9:reddit"
               ]
myPlacement = withGaps (16,0,16,0) (smart (0.5,0.5))


myLogHook dest = dynamicLogWithPP defaultPP
    { ppOutput = hPutStrLn dest
    , ppVisible = wrap "(" ")"
    }

myManageHook = composeAll 
    [ className =? "stalonetray"            --> doIgnore
    , className =? "Eclipse"                --> doFloat
    , className =? "VirtualBox"             --> doFloat
    , className =? "Mysql-workbench-bin"    --> doFloat
    ]
    <+> manageScratchPad
    <+> placeHook myPlacement

myLayoutHook = avoidStruts $ tiled ||| Mirror tiled ||| ThreeColMid 1 (3/100) (1/2) ||| Grid ||| Full
    where
        tiled = Tall nmaster delta ratio
        nmaster = 1
        ratio = 1/2
        delta = 3/100

manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)
    where
        h = 0.40         -- terminal height (40%)
        w = 0.6          -- terminal width (60%)
        t = 1 - h        -- distance from top edge (90%)
        l = 1 - w        -- distance from left edge (40%)

spawnScratch = scratchpadSpawnActionTerminal "urxvt"

main = do
    xmobar <- spawnPipe "xmobar"
    xmonad $ withUrgencyHook NoUrgencyHook defaultConfig
        { terminal              = myTerminal                                                                                -- Custom terminal
        , XMonad.workspaces     = myWorkspaces                                                                              -- Workspace list
        , startupHook           = spawn "/etc/rc.local"                                                                     -- start my default apps and set xrandr settings
        , normalBorderColor     = solarizedBase01
        , focusedBorderColor    = solarizedRed
        , borderWidth           = 3                                                                                         -- Set a small border
        , modMask               = mod4Mask                                                                                  -- Rebind to the windows key
        , manageHook            = manageDocks <+> myManageHook <+> manageHook defaultConfig                                 -- Add custom management hooks
        , layoutHook            = myLayoutHook
        , focusFollowsMouse     = False
        , logHook               = dynamicLogWithPP $ xmobarPP                                                               -- xmobar configuration
          { ppOutput  = hPutStrLn xmobar
            , ppCurrent = xmobarColor "#FFF7F5" "#4C646B" . wrap "[" "]"
            , ppVisible = wrap "(" ")"
            , ppUrgent  = xmobarColor "#992929" "#233138"
            , ppTitle   = xmobarColor "#A84B3A" "" .shorten 80
            }
    } `additionalKeys`
        ([ ((mod4Mask .|. shiftMask, xK_z    ),  spawn "xscreensaver-command -lock")
        , ((0,                      xK_Print),  spawn "scrot")
        , ((mod4Mask,               xK_m    ),  spawn "muter")
        , ((mod4Mask,               xK_v    ),  spawn "gvim")
        , ((mod4Mask,               xK_g    ),  spawn "google-chrome")
        , ((mod4Mask,               xK_n    ),  spawn "nemo")                                                                                            
        , ((mod4Mask,               xK_f    ),  spawn "firefox")                                                                                             
--        , ((mod4Mask,               xK_n    ),  spawn "nautilus")
        , ((mod4Mask,               xK_s    ),  spawnScratch)
        , ((mod1Mask,               xK_Tab  ),  windows W.focusDown)
--        , ((mod4Mask .|. shiftMask, xK_r    ),  goToSelected defaultGSConfig)
        , ((mod4Mask,               xK_a    ),  placeFocused myPlacement)
        , ((mod4Mask,		    xK_q    ),  spawn"killall redshift xscreensaver" >> restart "xmonad" True)
--	, ((mod4Mask,		    xK_F8   ), lowerVolume 3 >> return ())
--	, ((mod4Mask,		    xK_F9   ), raiseVolume 3 >> return ())
--	, ((mod4Mask, 		    xK_F10  ), toggleMute    >> return ())
        , ((mod4Mask,               xK_F11  ),  spawn "amixer -c1 sset Master 2-")
        , ((mod4Mask,               xK_F12  ),  spawn "amixer -c1 sset Master 2+")	
	]
        ++
        [((m .|. mod4Mask, key), screenWorkspace sc >>= flip whenJust (windows . f)) -- Replace 'mod1Mask' with your mod key of choice.
           | (key, sc) <- zip [xK_w, xK_e, xK_r] [1,0,2] -- was [0..] *** change to match your screen order ***
           , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]])
