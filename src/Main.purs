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

newtype Assignment = Assignment { assignee :: Creep, target :: String }

instance encodeJsonAssignment :: EncodeJson Assignment where
  encodeJson (Assignment ass) = fromObject $ M.fromFoldable
    [ Tuple "assignee" (fromString <<< Creep.name $ ass.assignee)
    , Tuple "target" (fromString ass.target)
    ]

main :: forall e. Eff (cmd :: CMD, console :: CONSOLE, tick :: TICK, time :: TIME, memory :: MEMORY | e) Unit
main = do
  game <- Game.getGameGlobal

  let creeps = Game.creeps game
  let spawn = M.lookup "Spawn1" $ Game.spawns game

  maybe (log "No spawn detected") doSpawnActions spawn
  assignments <- traverse doCreepActions creeps

  mem <- Memory.getMemoryGlobal
  Memory.set mem "assignments" (encodeJson assignments)

  pure unit

doCreepActions :: Creep -> forall e. Eff (cmd :: CMD, console :: CONSOLE, tick :: TICK, time :: TIME, memory :: MEMORY | e) (Maybe Assignment)
doCreepActions creep = do
  let room = RoomObject.room creep
  let sources = Room.find room find_sources

  let source = head sources

  maybe (log "No more sources" >>= (const $ pure Nothing)) (doCollectEnergy creep) source

doCollectEnergy :: Creep -> Source -> forall e. Eff (cmd :: CMD, console :: CONSOLE, tick :: TICK, time :: TIME, memory :: MEMORY | e) (Maybe Assignment)
doCollectEnergy creep source = do
  moveToSourceResult <- (Creep.moveTo creep (TargetObj source))

  when (moveToSourceResult /= ok) $ doLogReturnCode "MOVE_CREEP_TO_SOURCE" moveToSourceResult
  when (moveToSourceResult == ok) $ do
    harvestSourceResult <- Creep.harvestSource creep source
    when (harvestSourceResult /= ok) $ doLogReturnCode "HARVEST_SOURCE" harvestSourceResult

  pure <<< Just $ Assignment { assignee: creep, target: FFI.unsafeField "id" source }

doSpawnActions :: Spawn -> forall e. Eff (cmd :: CMD, console :: CONSOLE, tick :: TICK, time :: TIME | e) Unit
doSpawnActions spawn = do
  doCreateCreep spawn
  doLogSpawnEnergy spawn

doCreateCreep :: Spawn -> forall e. Eff (cmd :: CMD, console :: CONSOLE, tick :: TICK, time :: TIME | e) Unit
doCreateCreep spawn = do
  createCreepResult <- Spawn.createCreep spawn energyParts
  either (doLogReturnCode "CREATE_CREEP") (const $ pure unit) createCreepResult

doLogReturnCode :: String -> ReturnCode -> forall e. Eff (cmd :: CMD, console :: CONSOLE, tick :: TICK, time :: TIME | e) Unit
doLogReturnCode actionName returnCode = log $ "[FAILED " <> actionName <> "]:" <> show returnCode

doLogSpawnEnergy :: Spawn -> forall e. Eff (cmd :: CMD, console :: CONSOLE, tick :: TICK, time :: TIME | e) Unit
doLogSpawnEnergy spawn = log $ "Spawn " <> show (Spawn.name spawn) <> ": " <> show (Spawn.energy spawn)

energyParts :: Array BodyPartType
energyParts = [part_move, part_work, part_carry]
