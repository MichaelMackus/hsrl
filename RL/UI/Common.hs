module RL.UI.Common where

type Sprite   = (Point, String) -- horizontal string somewhere on the screen
type Point    = (Int, Int)
data Key      = KeyChar Char | KeyUp | KeyDown | KeyRight | KeyLeft |
                KeyEnter | KeyEscape | KeyBackspace |
                KeyMouseLeft Point | KeyMouseRight Point | KeyUnknown
    deriving (Show)

class Renderable r where
    getSprites :: r -> [Sprite]

-- game is renderable
-- instance Renderable (Game a) where
    -- getSprites g = getSprites (level g) ++ getMsgSprites (messages g)
    -- getSprites g = [((0, 0), show $ gets dungeon)]

class UI ui where
    -- initialize rendering
    uiInit   :: IO ui
    -- shut down
    uiEnd    :: ui -> IO ()
    -- main render function
    uiRender :: Renderable r => ui -> r -> IO ()
    -- wait on input from keyboard and return the single key inputted
    uiInput  :: ui -> IO Key
