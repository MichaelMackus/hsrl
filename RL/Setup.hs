module RL.Setup (defaultGame, setupGame) where

import RL.Game
import RL.IO
import RL.Map
import RL.Mob
import RL.Random
import RL.State

import Control.Applicative
import Control.Monad.State
import Data.Time.Clock.POSIX
import UI.HSCurses.Curses
import System.Random

-- defaults

defaultGame :: Game
defaultGame = Game {
    level = defaultMap,
    player = defaultPlayer,
    mobs = [],
    messages = [],
    seed = Nothing
}

defaultMap :: Map
defaultMap = toMap [
        "--------------------------------------------",
        "|..........................................|",
        "|..........................................|",
        "|..........................................|",
        "|..........................................|",
        "|..........................................|",
        "|..........................................|",
        "|..........................................|",
        "--------------------------------------------" ]
    where toMap = map toRow
          toRow = map tile

defaultMob = Mob {
    hp     = 10,
    symbol = undefined,
    at     = (-1, -1)
}

-- define some mobs
defaultPlayer = defaultMob { symbol = '@' }
orc           = defaultMob { symbol = 'o', hp = 7 }
kobold        = defaultMob { symbol = 'k', hp = 4 }
goblin        = defaultMob { symbol = 'g', hp = 2 }


-- default state setup
--
-- TODO cleanup

-- default game setup
setupGame :: GameState ()
setupGame = do
        io $ do                     -- setup basic tty
            initCurses
            cursSet CursorInvisible
            echo False
        defaultSeed >>= setSeed     -- seed the RNG
        ms   <- setupMobs           -- setup random mobs
        p    <- setupPlayer         -- setup player start
        game <- get
        put $ game { player = p, mobs = ms }
    where
        defaultSeed = io $ mkStdGen <$> roundTime -- sensible default
            where roundTime = round `fmap` getPOSIXTime

-- default mobs setup
setupMobs :: GameState [Mob]
setupMobs = mapM mapMob defaultMobs
    where
        mapMob        m = randomBlankPoint >>= moveMobTo' m
        moveMobTo'  m p = return $ moveMobTo p m
        defaultMobs     = [kobold, orc, goblin]

-- default player
setupPlayer :: GameState Player
setupPlayer = do
    p     <- getPlayer
    start <- randomBlankPoint
    return p { at = start }
