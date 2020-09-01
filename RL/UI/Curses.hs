{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module RL.UI.Curses (cursesUI) where

import RL.UI.Common

import Control.Monad (forM_, when)
import Data.Char
import qualified UI.HSCurses.Curses as Curses
import UI.HSCurses.CursesHelper (displayKey)

import Debug.Trace (trace)

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
            ch <- Curses.getch
            let k = case ch of
                    (Curses.KeyChar ch) -> if fromEnum ch == 27 then KeyEscape
                                           else if ch == '\DEL' then KeyBackspace
                                           else if ch == '\b'   then KeyBackspace
                                           else if ch == '\n'   then KeyEnter
                                           else KeyChar ch
                    -- TODO navigation keys not working
                    Curses.KeyUp        -> KeyUp
                    Curses.KeyDown      -> KeyDown
                    Curses.KeyRight     -> KeyRight
                    Curses.KeyLeft      -> KeyLeft
                    Curses.KeyEnter     -> KeyEnter
                    Curses.KeyBackspace -> KeyBackspace
                    -- Curses.KeyMouse     -> TODO
                    otherwise           -> KeyUnknown
            return (toKeyMods k)
      }

toKeyMods :: Key -> (Key, [KeyMod])
toKeyMods (KeyChar ch) = case displayKey (Curses.KeyChar ch) of
                            ('^':k:_) -> (KeyChar (toLower k), [KeyModCtrl])
                            otherwise -> let mods = if isUpper ch then [KeyModShift] else []
                                         in  (KeyChar ch, mods)
toKeyMods k = (k, [])
