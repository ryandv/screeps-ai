module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Control.Monad.State.Trans (StateT(..), runStateT)

import Data.Argonaut.Encode (encodeJson)
import Data.Array (catMaybes, concat, filter, head, tail, (:))
import Data.Either (Either, either)
import Data.Identity (Identity(..))
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.StrMap as M
import Data.Tuple (Tuple(..), fst, snd)
import Data.Traversable (Accum, foldl, mapAccumL, traverse)

import Screeps (Creep, ReturnCode)
import Screeps.Constants (find_sources)
import Screeps.Controller as Controller
import Screeps.Creep as Creep
import Screeps.Game as Game
import Screeps.Memory as Memory
import Screeps.Room as Room
import Screeps.RoomObject as RoomObject
import Screeps.RoomPosition as RoomPosition

import App.Execution as Execution
import Types

mainLoop :: Eff BaseScreepsEffects Unit
mainLoop = do
  reports <- generateReports
  let reportObservations = analyzeReports reports
  log $ show reportObservations

  state <- getStateFromMemory

  instructionQueue <- getInstructionQueue
  let creepInstructionQueue = catMaybes $ map head $ M.fold (\acc key val -> val:acc) [] $ getCreepInstructions state
  instructionResultObservations <- catMaybes <$> traverse Execution.executeInstruction (instructionQueue <> creepInstructionQueue)

  let instructionsAndNextState = (unwrap $ runStateT (StateT (generateInstructions (reportObservations <> instructionResultObservations))) state) :: Tuple (Array Instruction) AiState

  writeStateToMemory $ snd instructionsAndNextState
  writeInstructionsToQueue $ fst instructionsAndNextState

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

analyzeReports :: Reports -> Array Observation
analyzeReports (Reports
  { numberOfCreeps: numberOfCreeps
  , creepCapacities: creepCapacities
  , ticksToDowngrade: ticksToDowngrade
  , sourceLocations: sourceLocations
  , creepLocations: creepLocations
  , creepInstructions: creepInstructions
  , controllerLocation: controllerLocation
  }) = catMaybes
    [ if numberOfCreeps < 10 then (Just UnderCreepCap) else Nothing
    ]
      <> map SourceLocated sourceLocations
      <> catMaybes [ if ticksToDowngrade < 20000 then Just (ControllerIsLow controllerLocation) else Nothing ]
      <> creepFull creepCapacities
      <> creepEmpty creepCapacities
      <> catMaybes (map (toArrivedObservation creepLocations) (filter isCurrentlyMoving unfoldedCreepInstructions)) where

  unfoldedCreepInstructions :: Array (Tuple String (Array Instruction))
  unfoldedCreepInstructions = M.fold (\acc key val -> (Tuple key val):acc) [] creepInstructions

hasArrivedAtDestination :: M.StrMap Point -> Tuple String (Array Instruction) -> Boolean
hasArrivedAtDestination creepLocations (Tuple creepName instructions) = 1 >= chebyshevSquared
  (getCurrentPosition creepLocations creepName)
  (maybe (Point 0 0) (\instruction -> case instruction of
    MoveTo _ destination -> destination
    _ -> Point 0 0) $ head instructions)

getCurrentPosition :: M.StrMap Point -> String -> Point
getCurrentPosition creepLocations creepName = maybe (Point 0 0) id $ M.lookup creepName creepLocations

isCurrentlyMoving :: Tuple String (Array Instruction) -> Boolean
isCurrentlyMoving instructions = case head (snd instructions) of
                                        (Just (MoveTo _ _ )) -> true
                                        _ -> false

toArrivedObservation :: M.StrMap Point -> Tuple String (Array Instruction) -> Maybe Observation
toArrivedObservation creepLocations creepInstructions | hasArrivedAtDestination creepLocations creepInstructions = Just <<< Arrived $ fst creepInstructions
                                                      | otherwise = Nothing

creepFull :: M.StrMap (Tuple Int Int) -> Array Observation
creepFull creepCapacities = map (\(Tuple creepName _) -> CreepFull creepName) $ filter (\(Tuple creepName (Tuple carrying capacity)) -> carrying >= capacity) $ M.fold (\acc key val -> (Tuple key val):acc) [] creepCapacities

creepEmpty :: M.StrMap (Tuple Int Int) -> Array Observation
creepEmpty creepCapacities = map (\(Tuple creepName _) -> CreepEmpty creepName) $ filter (\(Tuple creepName (Tuple carrying _)) -> carrying == 0) $ M.fold (\acc key val -> (Tuple key val):acc) [] creepCapacities

getInstructionQueue :: Eff BaseScreepsEffects (Array Instruction)
getInstructionQueue = do
  mem <- Memory.getMemoryGlobal
  instructionQueue <- (Memory.get mem "instructionQueue") :: forall e. (EffScreepsCommand e) (Either String (Array Instruction))
  pure $ either (const []) id instructionQueue


getStateFromMemory :: Eff BaseScreepsEffects AiState
getStateFromMemory = do
  game <- Game.getGameGlobal
  let creeps = Game.creeps game

  mem <- Memory.getMemoryGlobal
  aiState <- (Memory.get mem "aiState") :: forall e. (EffScreepsCommand e) (Either String AiState)

  -- a bit hacky - merge in newly spawned Creeps

  pure $ either (const $ AiState { creepStates: map (const $ Idle) creeps, creepInstructions: map (const []) creeps }) (\(AiState state) -> (AiState state
                { creepStates = foldl (\acc creepName -> M.alter (maybe (Just Idle) Just) creepName acc) state.creepStates $ M.keys creeps
                , creepInstructions = foldl (\acc creepName -> M.alter (maybe (Just []) Just) creepName acc) state.creepInstructions $ M.keys creeps
                })) aiState

writeStateToMemory :: AiState -> Eff BaseScreepsEffects Unit
writeStateToMemory state = do
  mem <- Memory.getMemoryGlobal
  Memory.set mem "aiState" (encodeJson state)

writeInstructionsToQueue :: (Array Instruction) -> Eff BaseScreepsEffects Unit
writeInstructionsToQueue instructions = do
  mem <- Memory.getMemoryGlobal
  Memory.set mem "instructionQueue" (encodeJson instructions)

generateInstructions :: (Array Observation) -> AiState -> MyIdentity (Tuple (Array Instruction) AiState)
generateInstructions observations state = MyIdentity $ Identity $ Tuple (concat $ instructionsAndNextState.value) (instructionsAndNextState.accum) where

  instructionsAndNextState :: Accum AiState (Array (Array Instruction))
  instructionsAndNextState = mapAccumL respondToObservation state observations

respondToObservation :: AiState -> Observation -> Accum AiState (Array Instruction)
respondToObservation state CannotSpawnCreep = { accum: state, value: [] }
respondToObservation state UnderCreepCap = { accum: state, value: [SpawnCreep] }
respondToObservation state (SourceLocated point) = respondToSourceLocated state point
respondToObservation (AiState state) (Arrived creepName) =
  { accum: (AiState
    { creepStates: state.creepStates
    , creepInstructions: M.update (maybe Nothing Just <<< tail) creepName state.creepInstructions
    })
  , value: []
  }
respondToObservation (AiState state) (CreepFull creepName) =
  { accum: (AiState
    { creepStates: (M.update (const $ Just Transferring) creepName state.creepStates)
    , creepInstructions: M.update (\instructions -> case head instructions of
                                  (Just (HarvestSource _ _)) -> tail instructions
                                  _ -> Just instructions) creepName state.creepInstructions
    })
  , value: []
  }
respondToObservation (AiState state) (CreepEmpty creepName) =
  { accum: (AiState
    { creepStates: (M.update (const $ Just Idle) creepName state.creepStates)
    , creepInstructions: M.update (\instructions -> case head instructions of
                                  (Just (TransferEnergyTo _ _)) -> tail instructions
                                  _ -> Just instructions) creepName state.creepInstructions
    })
  , value: []
  }
respondToObservation state (ControllerIsLow controllerLocation) = respondToControllerIsLow state controllerLocation

respondToControllerIsLow :: AiState -> Point -> Accum AiState (Array Instruction)
respondToControllerIsLow (AiState state) controllerLocation = foldl (instructCreepToTransferToController controllerLocation) { accum: (AiState state), value: [] } transferringCreeps where
  transferringCreeps = filter (\creepName -> maybe false ((==) Transferring) (M.lookup creepName state.creepStates)) $ M.keys state.creepStates

instructCreepToTransferToController :: Point -> Accum AiState (Array Instruction) -> String -> Accum AiState (Array Instruction)
instructCreepToTransferToController controllerLocation { accum: (AiState state), value: instructions } creepName =
  { accum: (AiState
    { creepStates: (M.update (const $ Just Error) creepName state.creepStates)
    , creepInstructions: (M.update (const $ Just [MoveTo creepName controllerLocation, TransferEnergyTo creepName controllerLocation]) creepName state.creepInstructions)
    })
  , value: []
  }


respondToSourceLocated :: AiState -> Point -> Accum AiState (Array Instruction)
respondToSourceLocated (AiState state) point = foldl (instructCreepToHarvestSource point) { accum: (AiState state), value: [] } idleCreeps where
  idleCreeps = filter (\creepName -> maybe false ((==) Idle) (M.lookup creepName state.creepStates)) $ M.keys state.creepStates

instructCreepToHarvestSource :: Point -> Accum AiState (Array Instruction) -> String -> Accum AiState (Array Instruction)
instructCreepToHarvestSource pt { accum: (AiState state), value: instructions } creepName =
  { accum: (AiState
    { creepStates: (M.update (const $ Just Harvesting) creepName state.creepStates)
    , creepInstructions: (M.alter (const $ Just [MoveTo creepName pt, HarvestSource creepName pt]) creepName state.creepInstructions)
    })
  , value: []
  }


main :: Eff BaseScreepsEffects Unit
main = do
  mainLoop

  pure unit

doLogReturnCode :: String -> ReturnCode -> forall e. (EffScreepsCommand e) Unit
doLogReturnCode actionName returnCode = log $ "[FAILED " <> actionName <> "]:" <> show returnCode
