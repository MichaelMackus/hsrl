module RL.UI (
    UI(..),
    DefaultUI(..),
    uiInitDefault,
    Sprite,
    Renderable(..),
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
#if defined(vty)
type DefaultUI = Vty
#elif defined(hscurses)
type DefaultUI = Window
#else
type DefaultUI = ()
#endif

uiInitDefault = uiInit :: IO DefaultUI
