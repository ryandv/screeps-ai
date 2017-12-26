module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Except
import Control.Monad.Except.Trans
import Control.Monad.State
import Control.Monad.State.Trans

import Data.Argonaut.Core
import Data.Argonaut.Decode
import Data.Argonaut.Encode
import Data.Array
import Data.Either
import Data.Identity
import Data.Int as Int
import Data.Maybe
import Data.Newtype (class Newtype, unwrap)
import Data.Ord as Ord
import Data.Semigroup
import Data.Show
import Data.StrMap as M
import Data.Tuple
import Data.Traversable

import Screeps
import Screeps.Constants
import Screeps.Controller as Controller
import Screeps.Creep as Creep
import Screeps.FFI as FFI
import Screeps.Game as Game
import Screeps.Memory as Memory
import Screeps.Room as Room
import Screeps.RoomObject as RoomObject
import Screeps.RoomPosition as RoomPosition
import Screeps.Spawn as Spawn

data Point = Point Int Int

instance showPoint :: Show Point where
  show (Point x y) = "Point(" <> show x <> "," <> show y <> ")"
instance encodePoint :: EncodeJson Point where
  encodeJson (Point x y) = fromObject $ M.fromFoldable
    [ Tuple "x" $ fromNumber $ Int.toNumber x
    , Tuple "y" $ fromNumber $ Int.toNumber y
    ]

instance decodePoint :: DecodeJson Point where
  decodeJson point = do
    x <- getField (maybe (M.fromFoldable []) id (toObject point)) "x"
    y <- getField (maybe (M.fromFoldable []) id (toObject point)) "y"
    Right $ Point x y

chebyshevSquared :: Point -> Point -> Int
chebyshevSquared (Point x1 y1) (Point x2 y2) = max (Ord.abs $ x2 - x1) (Ord.abs $ y2 - y1)

data CreepState = Idle | Moving | Harvesting | Transferring | Error

data AiState = AiState
  { creepStates :: (M.StrMap CreepState)
  , creepInstructions :: (M.StrMap (Array Instruction))
  }

creepInstructions :: AiState -> M.StrMap (Array Instruction)
creepInstructions (AiState state) = state.creepInstructions

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

data Observation = UnderCreepCap | CannotSpawnCreep | SourceLocated Point | Arrived String | CreepFull String | ControllerIsLow Point

instance showObservation :: Show Observation where
  show UnderCreepCap = "UnderCreepCap"
  show CannotSpawnCreep = "UCannotSpawnCreep"
  show (SourceLocated pt) = "SourceLocated " <> show pt
  show (Arrived creepName) = "Arrived " <> creepName
  show (CreepFull creepName) = "CreepFull " <> creepName
  show (ControllerIsLow pt) = "ControllerIsLow " <> show pt

data Reports = Reports
  { numberOfCreeps :: Int
  , creepCapacities :: M.StrMap (Tuple Int Int)
  , ticksToDowngrade :: Int
  , sourceLocations :: Array Point
  , creepLocations :: M.StrMap Point
  , creepInstructions :: M.StrMap (Array Instruction)
  , controllerLocation :: Point
  }

data Instruction = SpawnCreep | DoLegacyHarvest String | MoveTo String Point | HarvestSource String Point

instance encodeInstruction :: EncodeJson Instruction where
  encodeJson SpawnCreep = fromObject $ M.fromFoldable
    [ Tuple "instruction_type" $ fromString "SpawnCreep"
    , Tuple "payload" $ fromObject $ M.fromFoldable []
    ]
  encodeJson (DoLegacyHarvest creepName) = fromObject $ M.fromFoldable
    [ Tuple "instruction_type" $ fromString "DoLegacyHarvest"
    , Tuple "payload" $ fromObject $ M.fromFoldable
      [ Tuple "creepName" $ fromString creepName ]
    ]
  encodeJson (MoveTo creepName pt) = fromObject $ M.fromFoldable
    [ Tuple "instruction_type" $ fromString "MoveTo"
    , Tuple "payload" $ fromObject $ M.fromFoldable
      [ Tuple "creepName" $ fromString creepName
      , Tuple "destination" $ encodeJson pt
      ]
    ]
  encodeJson (HarvestSource creepName pt) = fromObject $ M.fromFoldable
    [ Tuple "instruction_type" $ fromString "HarvestSource"
    , Tuple "payload" $ fromObject $ M.fromFoldable
      [ Tuple "creepName" $ fromString creepName
      , Tuple "sourceLocation" $ encodeJson pt
      ]
    ]

instance decodeInstruction :: DecodeJson Instruction where
  decodeJson instruction  = let instructionType = (getField (maybe (M.fromFoldable []) id (toObject instruction)) "instruction_type") in
                                case instructionType of
                                     (Right "SpawnCreep") -> Right SpawnCreep
                                     (Right "MoveTo") -> do
                                       payload <- getField (maybe (M.fromFoldable []) id (toObject instruction)) "payload"
                                       creepName <- getField payload "creepName"
                                       destination <- getField payload "destination"
                                       Right $ MoveTo creepName destination
                                     (Right "HarvestSource") -> do
                                       payload <- getField (maybe (M.fromFoldable []) id (toObject instruction)) "payload"
                                       creepName <- getField payload "creepName"
                                       sourceLocation <- getField payload "sourceLocation"
                                       Right $ HarvestSource creepName sourceLocation
                                     (Right _) -> Right $ DoLegacyHarvest ""
                                     (Left e) -> Left e

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
  let creepInstructionQueue = catMaybes $ map head $ M.fold (\acc key val -> val:acc) [] $ creepInstructions state
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
  let spawn = M.lookup "Spawn1" $ Game.spawns game
  let sources = maybe [] (\spawn -> Room.find (RoomObject.room spawn) find_sources) spawn

  let creepCapacities = map getCreepCapacity creeps

  pure $ Reports
    { numberOfCreeps: maybe 0 id <<< Int.fromNumber $ M.size creeps
    , creepCapacities: creepCapacities
    , ticksToDowngrade: maybe 0 (\spawn -> case Room.controller $ RoomObject.room spawn of
                                  Just controller -> Controller.ticksToDowngrade controller
                                  Nothing -> 0) spawn
    , sourceLocations: map (\source ->
                        let roomPos = (RoomObject.pos source) in
                            Point (RoomPosition.x roomPos) (RoomPosition.y roomPos)) sources
    , creepLocations: map (\creep -> Point (RoomPosition.x (RoomObject.pos creep)) (RoomPosition.y (RoomObject.pos creep))) creeps
    , creepInstructions: creepInstructions (either (const $ AiState { creepInstructions: M.empty, creepStates: M.empty }) id aiState)
    , controllerLocation: maybe (Point 0 0) (\spawn -> case Room.controller $ RoomObject.room spawn of
                                Just controller -> (Point (RoomPosition.x (RoomObject.pos controller)) (RoomPosition.y (RoomObject.pos controller)))
                                Nothing -> (Point 0 0)) spawn
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
      <> creepCapacityObservations creepCapacities
      <> catMaybes (map toArrivedObservation (filter isCurrentlyMoving unfoldedCreepInstructions)) where

    unfoldedCreepInstructions :: Array (Tuple String (Array Instruction))
    unfoldedCreepInstructions = M.fold (\acc key val -> (Tuple key val):acc) [] creepInstructions

    isCurrentlyMoving :: Tuple String (Array Instruction) -> Boolean
    isCurrentlyMoving instructions = case head (snd instructions) of
                                            (Just (MoveTo _ _ )) -> true
                                            _ -> false

    toArrivedObservation :: Tuple String (Array Instruction) -> Maybe Observation
    toArrivedObservation creepInstructions | hasArrivedAtDestination creepInstructions = Just <<< Arrived $ fst creepInstructions
                                           | otherwise = Nothing

    hasArrivedAtDestination :: Tuple String (Array Instruction) -> Boolean
    hasArrivedAtDestination (Tuple creepName instructions) = 1 >= chebyshevSquared
      (getCurrentPosition creepName)
      (maybe (Point 0 0) (\instruction -> case instruction of
        MoveTo _ destination -> destination
        _ -> Point 0 0) $ head instructions)

    creepCapacityObservations :: M.StrMap (Tuple Int Int) -> Array Observation
    creepCapacityObservations creepCapacities = map (\(Tuple creepName _) -> CreepFull creepName) $ filter (\(Tuple creepName (Tuple carrying capacity)) -> carrying >= capacity) $ M.fold (\acc key val -> (Tuple key val):acc) [] creepCapacities

    getCurrentPosition :: String -> Point
    getCurrentPosition creepName = maybe (Point 0 0) id $ M.lookup creepName creepLocations

getInstructionQueue :: Eff BaseScreepsEffects (Array Instruction)
getInstructionQueue = do
  mem <- Memory.getMemoryGlobal
  instructionQueue <- (Memory.get mem "instructionQueue") :: forall e. (EffScreepsCommand e) (Either String (Array Instruction))
  pure $ either (const []) id instructionQueue

executeInstruction :: Instruction -> Eff BaseScreepsEffects (Maybe Observation)
executeInstruction SpawnCreep = do
  game <- Game.getGameGlobal

  let spawn = M.lookup "Spawn1" $ Game.spawns game

  maybe (pure $ Just CannotSpawnCreep) (\spawn -> do
        createCreepResult <- Spawn.createCreep spawn energyParts
        pure $ either (const $ Just CannotSpawnCreep) (const Nothing) createCreepResult) spawn
executeInstruction (DoLegacyHarvest creepName) = pure Nothing
executeInstruction (MoveTo creepName (Point x y)) = do
  log $ "Executing MoveTo(" <> creepName <> "," <> show x <> "," <> show y <> ")"
  game <- Game.getGameGlobal
  observation <- runExceptT do
    let creep = M.lookup creepName $ Game.creeps game

    maybe (throwError UndistinguishedErrors) (\creep -> do
      doTryCommand "MOVE_CREEP" $ Creep.moveTo creep (TargetPt x y)
      pure $ Nothing) creep

  pure $ either (const Nothing) id $ observation
executeInstruction (HarvestSource creepName (Point x y)) = do
  log $ "Executing HarvestSource(" <> creepName <> "," <> show x <> "," <> show y <> ")"
  game <- Game.getGameGlobal
  observation <- runExceptT do
    let creep = M.lookup creepName $ Game.creeps game

    maybe (throwError UndistinguishedErrors) (\creep -> do
      let source = RoomPosition.lookFor (RoomPosition.mkRoomPosition x y (Room.name (RoomObject.room creep))) look_sources

      case source of
           (Right sources) -> maybe (pure $ Nothing) (\source -> do
             doTryCommand "HARVEST_SOURCE" $ Creep.harvestSource creep source
             pure Nothing) $ head sources
           _ -> pure $ Nothing

      ) creep

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
respondToObservation state (ControllerIsLow controllerLocation) = respondToControllerIsLow state controllerLocation

respondToControllerIsLow :: AiState -> Point -> Accum AiState (Array Instruction)
respondToControllerIsLow (AiState state) controllerLocation = foldl (instructCreepToTransferToController controllerLocation) { accum: (AiState state), value: [] } transferringCreeps where
  transferringCreeps = filter (\creepName -> maybe false ((==) Transferring) (M.lookup creepName state.creepStates)) $ M.keys state.creepStates

instructCreepToTransferToController :: Point -> Accum AiState (Array Instruction) -> String -> Accum AiState (Array Instruction)
instructCreepToTransferToController controllerLocation { accum: (AiState state), value: instructions } creepName =
  { accum: (AiState
    { creepStates: (M.update (const $ Just Idle) creepName state.creepStates)
    , creepInstructions: (M.update (const $ Just [MoveTo creepName controllerLocation]) creepName state.creepInstructions)
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
                            err_not_in_range -> doThrowError commandName result OutOfRangeError
                            err_not_enough_energy -> doThrowError commandName result OutOfEnergyError
                            err_not_enough_resources -> doThrowError commandName result OutOfResourcesError
                            _ -> doThrowError commandName result UndistinguishedErrors

doThrowError :: String -> ReturnCode -> CommandError -> forall e. ExceptT CommandError (Eff BaseScreepsEffects) Unit
doThrowError commandName result error = do
  throwError error

doLogReturnCode :: String -> ReturnCode -> forall e. (EffScreepsCommand e) Unit
doLogReturnCode actionName returnCode = log $ "[FAILED " <> actionName <> "]:" <> show returnCode

energyParts :: Array BodyPartType
energyParts = [part_move, part_work, part_carry]
