module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Array
import Data.Either
import Data.Maybe
import Data.Semigroup
import Data.StrMap as M
import Data.Tuple
import Data.Traversable

import Screeps
import Screeps.Constants
import Screeps.Creep as Creep
import Screeps.FFI as FFI
import Screeps.Game as Game
import Screeps.Memory as Memory
import Screeps.Room as Room
import Screeps.RoomObject as RoomObject
import Screeps.Spawn as Spawn

data CreepState = Idle | Harvesting | Full | Transferring | Error

type EffScreepsCommand e = Eff (cmd :: CMD, console :: CONSOLE, tick :: TICK, time :: TIME, memory :: MEMORY | e)

instance encodeCreepState :: EncodeJson CreepState where
  encodeJson Idle = fromString "Idle"
  encodeJson Harvesting = fromString "Harvesting"
  encodeJson Full = fromString "Full"
  encodeJson Transferring = fromString "Transferring"
  encodeJson Error = fromString "Error"

main :: forall e. (EffScreepsCommand e) Unit
main = do
  game <- Game.getGameGlobal

  let creeps = Game.creeps game
  let spawn = M.lookup "Spawn1" $ Game.spawns game

  maybe (log "No spawn detected") doSpawnActions spawn
  creepStates <- traverse doCreepActions creeps

  mem <- Memory.getMemoryGlobal
  Memory.set mem "creepStates" (encodeJson creepStates)

  pure unit

doCreepActions :: Creep -> forall e. (EffScreepsCommand e) CreepState
doCreepActions creep = do
  let room = RoomObject.room creep
  let sources = Room.find room find_sources

  let source = head sources

  maybe (log "No more sources" >>= (const $ pure Idle)) (doCollectEnergy creep) source

doCollectEnergy :: Creep -> Source -> forall e. (EffScreepsCommand e) CreepState
doCollectEnergy creep source = do
  moveToSourceResult <- (Creep.moveTo creep (TargetObj source))

  when (moveToSourceResult /= ok) $ doLogReturnCode "MOVE_CREEP_TO_SOURCE" moveToSourceResult
  when (moveToSourceResult == ok) $ do
    harvestSourceResult <- Creep.harvestSource creep source
    when (harvestSourceResult /= ok) $ doLogReturnCode "HARVEST_SOURCE" harvestSourceResult

  pure Harvesting

doSpawnActions :: Spawn -> forall e. (EffScreepsCommand e) Unit
doSpawnActions spawn = do
  doCreateCreep spawn
  doLogSpawnEnergy spawn

doCreateCreep :: Spawn -> forall e. (EffScreepsCommand e) Unit
doCreateCreep spawn = do
  createCreepResult <- Spawn.createCreep spawn energyParts
  either (doLogReturnCode "CREATE_CREEP") (const $ pure unit) createCreepResult

doLogReturnCode :: String -> ReturnCode -> forall e. (EffScreepsCommand e) Unit
doLogReturnCode actionName returnCode = log $ "[FAILED " <> actionName <> "]:" <> show returnCode

doLogSpawnEnergy :: Spawn -> forall e. (EffScreepsCommand e) Unit
doLogSpawnEnergy spawn = log $ "Spawn " <> show (Spawn.name spawn) <> ": " <> show (Spawn.energy spawn)

energyParts :: Array BodyPartType
energyParts = [part_move, part_work, part_carry]
