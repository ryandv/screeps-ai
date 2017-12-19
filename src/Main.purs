module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.Maybe
import Data.StrMap as M

import Screeps
import Screeps.Game as Game
import Screeps.Spawn as Spawn

main :: forall e. Eff (cmd :: CMD, console :: CONSOLE, tick :: TICK, time :: TIME | e) Unit
main = do
  game <- Game.getGameGlobal
  let spawn = M.lookup "Spawn1" $ Game.spawns game
  maybe
    (log "No spawn detected")
    (\spawn -> log $ "Spawn " <> show (Spawn.name spawn) <> "[" <> show (Spawn.energy spawn) <> "]")
    spawn
