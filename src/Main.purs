module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Except
import Control.Monad.Except.Trans

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

data CommandError = AllErrors

type EffScreepsCommand e = Eff (cmd :: CMD, console :: CONSOLE, tick :: TICK, time :: TIME, memory :: MEMORY | e)

type BaseScreepsEffects = (cmd :: CMD, console :: CONSOLE, tick :: TICK, time :: TIME, memory :: MEMORY)

instance encodeCreepState :: EncodeJson CreepState where
  encodeJson Idle = fromString "Idle"
  encodeJson Harvesting = fromString "Harvesting"
  encodeJson Full = fromString "Full"
  encodeJson Transferring = fromString "Transferring"
  encodeJson Error = fromString "Error"

main :: Eff BaseScreepsEffects Unit
main = do
  game <- Game.getGameGlobal

  let creeps = Game.creeps game
  let spawn = M.lookup "Spawn1" $ Game.spawns game

  maybe (log "No spawn detected") doSpawnActions spawn
  creepStates <- traverse doCreepAction creeps

  mem <- Memory.getMemoryGlobal
  Memory.set mem "creepStates" (encodeJson creepStates)

  pure unit

doSpawnActions :: Spawn -> forall e. (EffScreepsCommand e) Unit
doSpawnActions spawn = do
  doCreateCreep spawn
  doLogSpawnEnergy spawn

doCreateCreep :: Spawn -> forall e. (EffScreepsCommand e) Unit
doCreateCreep spawn = do
  createCreepResult <- Spawn.createCreep spawn energyParts
  either (doLogReturnCode "CREATE_CREEP") (const $ pure unit) createCreepResult

doLogSpawnEnergy :: Spawn -> forall e. (EffScreepsCommand e) Unit
doLogSpawnEnergy spawn = log $ "Spawn " <> show (Spawn.name spawn) <> ": " <> show (Spawn.energy spawn)

doCreepAction :: Creep -> Eff BaseScreepsEffects CreepState
doCreepAction creep = do
  let room = RoomObject.room creep
  let sources = Room.find room find_sources

  let source = head sources

  maybe (log "No more sources" >>= (const $ pure Idle)) (doCollectEnergy creep) source

doCollectEnergy :: Creep -> Source -> Eff BaseScreepsEffects CreepState
doCollectEnergy creep source = (either (const $ pure Error) (const $ pure Harvesting)) =<< (runExceptT $ doMoveAndHarvest creep source)

doMoveAndHarvest :: Creep -> Source -> ExceptT CommandError (Eff BaseScreepsEffects) Unit
doMoveAndHarvest creep source = do
  doTryCommand "MOVE_CREEP_TO_SOURCE" $ Creep.moveTo creep (TargetObj source)
  doTryCommand "HARVEST_SOURCE" $ Creep.harvestSource creep source

doTryCommand :: String -> Eff BaseScreepsEffects ReturnCode -> ExceptT CommandError (Eff BaseScreepsEffects) Unit
doTryCommand commandName command = do
 result <- (liftEff command)
 when (result == ok) $ (pure unit)
 when (result /= ok) $ doThrowError commandName result

doThrowError :: String -> ReturnCode -> forall e. ExceptT CommandError (Eff BaseScreepsEffects) Unit
doThrowError commandName result = do
  liftEff $ (doLogReturnCode commandName result :: (Eff (cmd :: CMD, console :: CONSOLE, tick :: TICK, time :: TIME, memory :: MEMORY) Unit))
  throwError AllErrors

doLogReturnCode :: String -> ReturnCode -> forall e. (EffScreepsCommand e) Unit
doLogReturnCode actionName returnCode = log $ "[FAILED " <> actionName <> "]:" <> show returnCode

energyParts :: Array BodyPartType
energyParts = [part_move, part_work, part_carry]
