{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module RL.UI.Curses where

import RL.UI.Common

import Control.Monad (forM_)
import qualified UI.HSCurses.Curses as Curses

instance UI Curses.Window where
    uiInit = do
        Curses.initCurses
        Curses.echo False
        Curses.cursSet Curses.CursorInvisible
        return Curses.stdScr
    uiRender w r = do
        let sprites = getSprites r
        Curses.wclear w
        forM_ sprites $ \((x,y), str) -> do
            Curses.mvWAddStr w y x str
        Curses.refresh
    uiEnd w = Curses.endWin
    uiInput w = do
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
