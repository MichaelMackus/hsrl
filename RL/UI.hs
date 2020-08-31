module RL.UI (
    UI(..),
    defaultUI,
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
import RL.UI.Raw

-- default display implementation
defaultUI :: UIConfig -> IO UI
#if defined(sdl)
defaultUI = sdlUI
#elif defined(vty)
defaultUI = vtyUI
#elif defined(hscurses)
defaultUI = cursesUI
#else
-- fallback using putStrLn
defaultUI = rawUI
#endif

defaultUIConfig = UIConfig { columns = 80
                           , rows = 24
                           , initMouse = False
                           , uiTitle = "RogueLike Demo"
                           , fontPath = "res/fonts/16x16x.fnt"
                           , fontSize = 16
                           , fullscreen = False }
