module RL.Generator.Dungeon (module RL.Generator, module RL.Generator.Dungeon) where

import RL.Map
import RL.Generator
import RL.Generator.DLevel

import Control.Monad.Reader (ask)
import Control.Monad.Random (getSplit)

dgenerator :: Generator s Dungeon
dgenerator = do
        conf <- ask
        g    <- getSplit
        let (lvl, g', _) = runGenerator levelGenerator conf g

        -- ensure we only generate the dungeon once, TODO check dungeon dimensions
        markGDone

        return (DTip lvl)
