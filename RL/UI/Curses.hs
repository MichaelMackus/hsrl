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
            (Curses.KeyChar ch) -> return (KeyChar ch)
            otherwise           -> return KeyUnknown
