module RL.Setup (defaultGame, setupGame) where

import RL.Dice
import RL.Game
import RL.IO

import Control.Applicative
import Control.Monad.State
import Data.Time.Clock.POSIX
import System.Random

-- defaults

defaultGame :: Game
defaultGame = Game {
    level = Level {
        tiles  = defaultMap,
        player = defaultPlayer,
        mobs   = []
    },
    messages = [],
    seed     = Nothing
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
    mobId  = 0,
    hp     = 10,
    symbol = undefined,
    at     = (-1, -1),
    dmgd   = 1 `d` 4
}

-- define some mobs
defaultPlayer = defaultMob { symbol = '@' }
orc           = defaultMob { symbol = 'o', hp = 7 }
kobold        = defaultMob { symbol = 'k', hp = 4, dmgd = 1 `d` 3 }
goblin        = defaultMob { symbol = 'g', hp = 2, dmgd = 1 `d` 2 }

-- default game setup
setupGame :: GameState ()
setupGame = do
    setupRNG                -- seed the RNG
    setupMobs               -- setup random mobs
    setupPlayer             -- setup player start

-- seed the RNG
setupRNG :: GameState ()
setupRNG = do
        s <- defaultSeed
        setSeed s
    where
        defaultSeed = io $ mkStdGen <$> roundTime       -- sensible default
            where roundTime = round `fmap` getPOSIXTime -- based on sys time

-- default mobs setup
setupMobs :: GameState ()
setupMobs = do
        ms <- mapM mapMob $ withMobIds defaultMobs
        setMobs ms
    where
        mapMob        m = randomBlankPoint >>= moveMobTo' m
        moveMobTo'  m p = return $ moveMobTo p m
        defaultMobs     = [kobold, orc, goblin, goblin]

-- default player
setupPlayer :: GameState ()
setupPlayer = do
    p     <- getPlayer
    start <- randomBlankPoint
    setPlayer $ p { at = start }
