{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module RL.UI.Curses (cursesUI) where

import RL.UI.Common

import Control.Monad (forM_, when)
import qualified UI.HSCurses.Curses as Curses

cursesUI :: UIConfig -> IO UI
cursesUI cfg = do
  Curses.initCurses
  (curRows, curCols) <- Curses.scrSize
  when (curRows < rows cfg || curCols < columns cfg)
      $ error "Terminal too small for curses window!"
  Curses.echo False
  Curses.cursSet Curses.CursorInvisible
  return UI
      { uiRender = \sprites -> do
            Curses.wclear Curses.stdScr
            forM_ sprites $ \((x,y), str) -> do
                Curses.mvWAddStr Curses.stdScr y x str
            Curses.refresh
      , uiEnd = Curses.endWin
      , uiInput = do
            ch <- Curses.getCh
            case ch of
                (Curses.KeyChar ch) -> if fromEnum ch == 27 then return KeyEscape
                                       else if ch == '\DEL' then return KeyBackspace
                                       else if ch == '\b'   then return KeyBackspace
                                       else if ch == '\n'   then return KeyEnter
                                       else return (KeyChar ch)
                Curses.KeyUp        -> return KeyUp
                Curses.KeyDown      -> return KeyDown
                Curses.KeyRight     -> return KeyRight
                Curses.KeyLeft      -> return KeyLeft
                Curses.KeyEnter     -> return KeyEnter
                Curses.KeyBackspace -> return KeyBackspace
                -- Curses.KeyMouse     -> TODO
                otherwise           -> return KeyUnknown
      }
