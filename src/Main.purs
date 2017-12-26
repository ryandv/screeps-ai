module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Control.Monad.State.Trans (StateT(..), runStateT)

import Data.Array (catMaybes, head, (:))
import Data.Either (Either, either)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.StrMap as M
import Data.Tuple (Tuple(..), fst, snd)
import Data.Traversable (traverse)

import Screeps (Creep)
import Screeps.Constants (find_sources)
import Screeps.Controller as Controller
import Screeps.Creep as Creep
import Screeps.Game as Game
import Screeps.Memory as Memory
import Screeps.Room as Room
import Screeps.RoomObject as RoomObject
import Screeps.RoomPosition as RoomPosition

import App.Execution as App.Execution
import App.Memory as App.Memory

import AI.Observations as AI.Observations
import AI.StateManager as AI.StateManager

import Types

main :: Eff BaseScreepsEffects Unit
main = do
  reports <- generateReports
  let reportObservations = AI.Observations.analyzeReports reports
  log $ show reportObservations

  state <- App.Memory.getStateFromMemory

  instructionQueue <- App.Memory.getInstructionQueue
  let creepInstructionQueue = catMaybes $ map head $ M.fold (\acc key val -> val:acc) [] $ getCreepInstructions state
  instructionResultObservations <- catMaybes <$> traverse App.Execution.executeInstruction (instructionQueue <> creepInstructionQueue)

  let instructionsAndNextState = (unwrap $ runStateT (StateT (AI.StateManager.generateInstructions (reportObservations <> instructionResultObservations))) state) :: Tuple (Array Instruction) AiState

  App.Memory.writeStateToMemory $ snd instructionsAndNextState
  App.Memory.writeInstructionsToQueue $ fst instructionsAndNextState

  pure unit

getCreepCapacity :: Creep -> Tuple Int Int
getCreepCapacity creep = Tuple (Creep.totalAmtCarrying creep) (Creep.carryCapacity creep)

generateReports :: Eff BaseScreepsEffects Reports
generateReports = do
  game <- Game.getGameGlobal
  mem <- Memory.getMemoryGlobal
  -- Hmm...
  aiState <- (Memory.get mem "aiState") :: forall e. (EffScreepsCommand e) (Either String AiState)

  let creeps = Game.creeps game
  let maybeSpawn = M.lookup "Spawn1" $ Game.spawns game
  let sources = maybe [] (\spawn -> Room.find (RoomObject.room spawn) find_sources) maybeSpawn

  let creepCapacities = map getCreepCapacity creeps

  pure $ Reports
    { numberOfCreeps: maybe 0 id <<< Int.fromNumber $ M.size creeps
    , creepCapacities: creepCapacities
    , ticksToDowngrade: maybe 0 (\spawn -> case Room.controller $ RoomObject.room spawn of
                                  Just controller -> Controller.ticksToDowngrade controller
                                  Nothing -> 0) maybeSpawn
    , sourceLocations: map (\source ->
                        let roomPos = (RoomObject.pos source) in
                            Point (RoomPosition.x roomPos) (RoomPosition.y roomPos)) sources
    , creepLocations: map (\creep -> Point (RoomPosition.x (RoomObject.pos creep)) (RoomPosition.y (RoomObject.pos creep))) creeps
    , creepInstructions: getCreepInstructions (either (const $ AiState { creepInstructions: M.empty, creepStates: M.empty }) id aiState)
    , controllerLocation: maybe (Point 0 0) (\spawn -> case Room.controller $ RoomObject.room spawn of
                                Just controller -> (Point (RoomPosition.x (RoomObject.pos controller)) (RoomPosition.y (RoomObject.pos controller)))
                                Nothing -> (Point 0 0)) maybeSpawn
    }
