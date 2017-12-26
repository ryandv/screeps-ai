module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Except.Trans (ExceptT, runExceptT, throwError)
import Control.Monad.State.Trans (StateT(..), runStateT)

import Data.Argonaut.Core (fromObject, fromString, toObject, toString)
import Data.Argonaut.Decode (class DecodeJson, getField)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Array (catMaybes, concat, filter, head, tail, (:))
import Data.Either (Either(..), either)
import Data.Identity (Identity(..))
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.StrMap as M
import Data.Tuple (Tuple(..), fst, snd)
import Data.Traversable (Accum, foldl, mapAccumL, traverse)

import Screeps (BodyPartType, CMD, Creep, MEMORY, ReturnCode, TICK, TIME, TargetPosition(..))
import Screeps.Constants (find_sources, look_sources, ok, part_carry, part_move, part_work, resource_energy)
import Screeps.Controller as Controller
import Screeps.Creep as Creep
import Screeps.Game as Game
import Screeps.Memory as Memory
import Screeps.Room as Room
import Screeps.RoomObject as RoomObject
import Screeps.RoomPosition as RoomPosition
import Screeps.Spawn as Spawn

import Types

data CreepState = Idle | Moving | Harvesting | Transferring | Error

data AiState = AiState
  { creepStates :: (M.StrMap CreepState)
  , creepInstructions :: (M.StrMap (Array Instruction))
  }

getCreepInstructions :: AiState -> M.StrMap (Array Instruction)
getCreepInstructions (AiState state) = state.creepInstructions

instance encodeAiState :: EncodeJson AiState where
  encodeJson (AiState { creepStates: creepStates, creepInstructions: creepInstructions }) = fromObject $ M.fromFoldable
    [ Tuple "creepStates" $ encodeJson creepStates
    , Tuple "creepInstructions" $ encodeJson creepInstructions
    ]

instance decodeAiState :: DecodeJson AiState where
  decodeJson json = do
    creepStates <- getField (maybe (M.fromFoldable []) id (toObject json)) "creepStates"
    creepInstructions <- getField (maybe (M.fromFoldable []) id (toObject json)) "creepInstructions"
    pure $ AiState { creepStates: creepStates, creepInstructions: creepInstructions }

instance showCreepState :: Show CreepState where
  show Idle = "Idle"
  show Moving = "Moving"
  show Harvesting = "Harvesting"
  show Transferring = "Transferring"
  show Error = "Error"

derive instance eqCreepState :: Eq CreepState

data CommandError = UndistinguishedErrors | OutOfRangeError | OutOfEnergyError | OutOfResourcesError

derive instance eqCommandError :: Eq CommandError

type EffScreepsCommand e = Eff (cmd :: CMD, console :: CONSOLE, tick :: TICK, time :: TIME, memory :: MEMORY | e)

type BaseScreepsEffects = (cmd :: CMD, console :: CONSOLE, tick :: TICK, time :: TIME, memory :: MEMORY)

newtype MyIdentity a = MyIdentity (Identity a)

instance newtypeIdentity :: Newtype (MyIdentity a) a where
  wrap a = (MyIdentity (Identity a))
  unwrap (MyIdentity (Identity a)) = a

instance encodeCreepState :: EncodeJson CreepState where
  encodeJson Idle = fromString "Idle"
  encodeJson Moving = fromString "Moving"
  encodeJson Harvesting = fromString "Harvesting"
  encodeJson Transferring = fromString "Transferring"
  encodeJson Error = fromString "Error"

instance decodeCreepState :: DecodeJson CreepState where
  decodeJson j | toString j == Just "Idle" = Right Idle
               | toString j == Just "Moving" = Right Moving
               | toString j == Just "Harvesting" = Right Harvesting
               | toString j == Just "Transferring" = Right Transferring
               | toString j == Just "Error" = Right Error
               | otherwise = Left "Failed to parse creepState"

mainLoop :: Eff BaseScreepsEffects Unit
mainLoop = do
  reports <- generateReports
  let reportObservations = analyzeReports reports
  log $ show reportObservations

  state <- getStateFromMemory

  instructionQueue <- getInstructionQueue
  let creepInstructionQueue = catMaybes $ map head $ M.fold (\acc key val -> val:acc) [] $ getCreepInstructions state
  instructionResultObservations <- catMaybes <$> traverse executeInstruction (instructionQueue <> creepInstructionQueue)

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

executeInstruction :: Instruction -> Eff BaseScreepsEffects (Maybe Observation)
executeInstruction (DoNothing creepName) = pure Nothing
executeInstruction SpawnCreep = do
  game <- Game.getGameGlobal

  let maybeSpawn = M.lookup "Spawn1" $ Game.spawns game

  maybe (pure $ Just CannotSpawnCreep) (\spawn -> do
        createCreepResult <- Spawn.createCreep spawn energyParts
        pure $ either (const $ Just CannotSpawnCreep) (const Nothing) createCreepResult) maybeSpawn
executeInstruction (MoveTo creepName (Point x y)) = do
  log $ "Executing MoveTo(" <> creepName <> "," <> show x <> "," <> show y <> ")"
  game <- Game.getGameGlobal
  observation <- runExceptT do
    let maybeCreep = M.lookup creepName $ Game.creeps game

    maybe (throwError UndistinguishedErrors) (\creep -> do
      doTryCommand "MOVE_CREEP" $ Creep.moveTo creep (TargetPt x y)
      pure $ Nothing) maybeCreep

  pure $ either (const Nothing) id $ observation
executeInstruction (HarvestSource creepName (Point x y)) = do
  log $ "Executing HarvestSource(" <> creepName <> "," <> show x <> "," <> show y <> ")"
  game <- Game.getGameGlobal
  observation <- runExceptT do
    let maybeCreep = M.lookup creepName $ Game.creeps game

    maybe (throwError UndistinguishedErrors) (\creep -> do
      let eitherSource = RoomPosition.lookFor (RoomPosition.mkRoomPosition x y (Room.name (RoomObject.room creep))) look_sources

      case eitherSource of
           (Right sources) -> maybe (pure $ Nothing) (\source -> do
             doTryCommand "HARVEST_SOURCE" $ Creep.harvestSource creep source
             pure Nothing) $ head sources
           _ -> pure $ Nothing

      ) maybeCreep

  pure $ either (const Nothing) id $ observation
executeInstruction (TransferEnergyTo creepName (Point x y)) = do
  log $ "Executing TransferEnergyTo(" <> creepName <> "," <> show x <> "," <> show y <> ")"
  game <- Game.getGameGlobal
  observation <- runExceptT do
    let maybeCreep = M.lookup creepName $ Game.creeps game

    maybe (throwError UndistinguishedErrors) (\creep ->
      case Room.controller (RoomObject.room creep) of
        Just controller -> do
          doTryCommand "TRANSFER_ENERGY_TO_CONTROLLER" $ Creep.transferToStructure creep controller resource_energy
          pure Nothing
        _ -> pure $ Nothing) maybeCreep

  pure $ either (const Nothing) id $ observation

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

doTryCommand :: String -> Eff BaseScreepsEffects ReturnCode -> ExceptT CommandError (Eff BaseScreepsEffects) Unit
doTryCommand commandName command = do
 result <- (liftEff command)
 when (result == ok) $ (pure unit)
 when (result /= ok) $ case result of
                            err_not_enough_energy -> doThrowError commandName result OutOfEnergyError
                            err_not_enough_resources -> doThrowError commandName result OutOfResourcesError
                            err_not_in_range -> doThrowError commandName result OutOfRangeError
                            _ -> doThrowError commandName result UndistinguishedErrors

doThrowError :: String -> ReturnCode -> CommandError -> ExceptT CommandError (Eff BaseScreepsEffects) Unit
doThrowError commandName result error = do
  throwError error

doLogReturnCode :: String -> ReturnCode -> forall e. (EffScreepsCommand e) Unit
doLogReturnCode actionName returnCode = log $ "[FAILED " <> actionName <> "]:" <> show returnCode

energyParts :: Array BodyPartType
energyParts = [part_move, part_work, part_carry]
