module RL.Setup (
    runGame,
    defaultGame
) where

-- This module defines some helpful initial defaults.
--
-- Exposes "runGame" black box IO function which sets up the Reader & State
-- monads.

import RL.Dice
import RL.Game
import RL.IO
import RL.Renderer

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Data.Time.Clock.POSIX
import System.Random

-- run game in black box IO action
--
-- This initializes the Reader and State in order to give the gameLoop a
-- renderer & persistent game state.  You can call any function which runs
-- within GameState with this.
--
-- For example: "runGame defaultGame (roll $ 1 `d` 20)" will generate a random
-- IO Int from 1-20 (albeit rather inefficiently ;))
runGame :: Game -> GameState r -> IO r
runGame g gs = do
        vty    <- mkRenderer               -- initialize VTY renderer
        (r, s) <- runReaderT gameState vty -- run the Renderer
        return r
    where
        -- run game                       initial Game
        gameState  = runStateT gameState' g
        -- wrap        (pre)   gameLoop  (post)
        gameState' = setupGame >> gs >>= shutdown

-- default game setup
setupGame :: GameState ()
setupGame = do
    setupRNG                -- seed the RNG
    -- todo
    -- setupDungeon            -- setup random dungeon
    setupMobs               -- setup random mobs
    setupPlayer             -- setup player start

-- shutdown the game
--
-- Sends helpful output of last Game Message, and returns the result var
-- from state unmodified.
shutdown :: r -> GameState r
shutdown r = do
    disp <- ask
    msgs <- getMessages
    io $ killRenderer disp                         -- shutdown display
    unless (null msgs) (io $ putStrLn (head msgs)) -- last game message
    return r                                       -- unmodified result

-- defaults

defaultGame :: Game
defaultGame = Game {
    level = Level {
        tiles  = defaultTiles,
        player = defaultPlayer,
        mobs   = []
    },
    messages = [],
    seed     = Nothing
}

defaultTiles :: Tiles
defaultTiles = toTiles [
        "--------------------------------------------",
        "|..........................................|",
        "|..........................................|",
        "|..........................................|",
        "|..........................................|",
        "|..........................................|",
        "|..........................................|",
        "|..........................................|",
        "--------------------------------------------" ]
    where toTiles = map toRow
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

-- setup helpers

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
