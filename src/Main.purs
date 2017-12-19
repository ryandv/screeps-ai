module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.Either
import Data.Maybe
import Data.StrMap as M

import Screeps
import Screeps.Constants
import Screeps.Game as Game
import Screeps.Spawn as Spawn

main :: forall e. Eff (cmd :: CMD, console :: CONSOLE, tick :: TICK, time :: TIME | e) Unit
main = do
  game <- Game.getGameGlobal
  let spawn = M.lookup "Spawn1" $ Game.spawns game
  maybe (log "No spawn detected") doSpawnActions spawn

doSpawnActions :: Spawn -> forall e. Eff (cmd :: CMD, console :: CONSOLE, tick :: TICK, time :: TIME | e) Unit
doSpawnActions spawn = do
  when (Spawn.canCreateCreep spawn [part_move, part_work, part_carry] == ok) $ do
    createCreepResult <- Spawn.createCreep spawn [part_move, part_work, part_carry]
    either (\returnCode -> (log $ "Failed to create creep: " <> show returnCode)) (const $ pure unit) createCreepResult
  log $ "Spawn " <> show (Spawn.name spawn) <> ": " <> show (Spawn.energy spawn)
