module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Screeps
import Screeps.Game as Game

main :: forall e. Eff (cmd :: CMD, console :: CONSOLE, tick :: TICK, time :: TIME | e) Unit
main = do
  game <- Game.getGameGlobal
  startUsed <- Game.getUsed game
  log $ "Start: " <> show startUsed
