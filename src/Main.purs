module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.Either
import Data.Maybe
import Data.StrMap as M
import Data.Traversable

import Screeps
import Screeps.Constants
import Screeps.Game as Game
import Screeps.Spawn as Spawn

main :: forall e. Eff (cmd :: CMD, console :: CONSOLE, tick :: TICK, time :: TIME | e) Unit
main = do
  game <- Game.getGameGlobal

  let creeps = Game.creeps game
  let spawn = M.lookup "Spawn1" $ Game.spawns game

  maybe (log "No spawn detected") doSpawnActions spawn
  traverse doCreepActions creeps

  pure unit

doCreepActions :: Creep -> forall e. Eff (cmd :: CMD, console :: CONSOLE, tick :: TICK, time :: TIME | e) Unit
doCreepActions creep = pure unit

doSpawnActions :: Spawn -> forall e. Eff (cmd :: CMD, console :: CONSOLE, tick :: TICK, time :: TIME | e) Unit
doSpawnActions spawn = do
  doCreateCreep spawn
  doLogSpawnEnergy spawn

doCreateCreep :: Spawn -> forall e. Eff (cmd :: CMD, console :: CONSOLE, tick :: TICK, time :: TIME | e) Unit
doCreateCreep spawn = do
  createCreepResult <- Spawn.createCreep spawn energyParts
  either doLogReturnCode (const $ pure unit) createCreepResult

doLogReturnCode :: ReturnCode -> forall e. Eff (cmd :: CMD, console :: CONSOLE, tick :: TICK, time :: TIME | e) Unit
doLogReturnCode returnCode = log $ "Failed to create creep: " <> show returnCode

doLogSpawnEnergy :: Spawn -> forall e. Eff (cmd :: CMD, console :: CONSOLE, tick :: TICK, time :: TIME | e) Unit
doLogSpawnEnergy spawn = log $ "Spawn " <> show (Spawn.name spawn) <> ": " <> show (Spawn.energy spawn)

energyParts :: Array BodyPartType
energyParts = [part_move, part_work, part_carry]
