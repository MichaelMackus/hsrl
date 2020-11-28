module RL.UI (
    UI(..),
    initDefaultUI,
    initTTYUI,
    Key(..),
    module RL.UI.Common
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
#if defined(vty)
import RL.UI.Vty
import Graphics.Vty (Vty)
#endif

-- default display implementation
initDefaultUI :: UIConfig -> IO UI
#if defined(sdl)
initDefaultUI = initSdlUI
#elif defined(vty)
initDefaultUI = vtyUI
#else
initDefaultUI = const (error "No display implementation found!")
#endif

#if defined(vty)
initTTYUI = vtyUI
#else
initTTYUI = const (error "Terminal UI is unsupported! Game is not compiled with the `vty` flag.")
#endif
