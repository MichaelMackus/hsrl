module RL.UI (
    UI(..),
    initUI,
    defaultUIConfig,
    Sprite,
    Key(..)
) where

import RL.UI.Common

-- Basic UI abstraction.
--
-- The master "render" function takes a Renderable thing and the Display to
-- output onto.
--
-- The Renderable thing returns Sprites through "getSprites" which are strings
-- somewhere on the screen.
--
-- See RL.UI.Common for the abstraction, or RL.UI.* for the
-- implementations.

#if defined(sdl)
import RL.UI.SDL
#endif
#if defined(hscurses)
import RL.UI.Curses
import UI.HSCurses.Curses (Window)
#endif
#if defined(vty)
import RL.UI.Vty
import Graphics.Vty (Vty)
#endif

-- default display implementation
initUI :: UIConfig -> IO UI
#if defined(sdl)
initUI = initSdlUI
#elif defined(vty)
initUI = vtyUI
#elif defined(hscurses)
initUI = cursesUI
#else
initUI = undefined
#endif

defaultUIConfig = UIConfig { columns = 80
                           , rows = 24
                           , initMouse = False
                           , uiTitle = "RogueLike Demo"
                           , fontPath = "res/fonts/16x16xw.woff"
                           , fontSize = 16
                           , fullscreen = False }
