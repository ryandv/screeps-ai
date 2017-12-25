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

data CreepState = Idle | Harvesting | Transferring | Error

data AiState = AiState
  { creepStates :: (M.StrMap CreepState)
  }

instance encodeAiState :: EncodeJson AiState where
  encodeJson (AiState { creepStates: creepStates }) = fromObject $ M.fromFoldable
    [ Tuple "creepStates" $ encodeJson creepStates
    ]

data Observation = UnderCreepCap | CannotSpawnCreep | SourceLocated Point
data Reports = Reports
  { numberOfCreeps :: Int
  , creepCapacities :: M.StrMap (Tuple Int Int)
  , ticksToDowngrade :: Int
  , sourceLocations :: Array Point
  }

data Instruction = SpawnCreep | DoLegacyHarvest String | HarvestEnergy String Point

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
  encodeJson (HarvestEnergy creepName pt) = fromObject $ M.fromFoldable
    [ Tuple "instruction_type" $ fromString "HarvestEnergy"
    , Tuple "payload" $ fromObject $ M.fromFoldable
      [ Tuple "creepName" $ fromString creepName
      , Tuple "destination" $ encodeJson pt
      ]
    ]

instance decodeInstruction :: DecodeJson Instruction where
  decodeJson instruction  = let instructionType = (getField (maybe (M.fromFoldable []) id (toObject instruction)) "instruction_type") in
                                case instructionType of
                                     (Right "SpawnCreep") -> Right SpawnCreep
                                     (Right "HarvestEnergy") -> do
                                       payload <- getField (maybe (M.fromFoldable []) id (toObject instruction)) "payload"
                                       creepName <- getField payload "creepName"
                                       destination <- getField payload "destination"
                                       Right $ HarvestEnergy creepName destination
                                     (Right _) -> Right $ DoLegacyHarvest ""
                                     (Left e) -> Left e

instance showCreepState :: Show CreepState where
  show Idle = "Idle"
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
  encodeJson Harvesting = fromString "Harvesting"
  encodeJson Transferring = fromString "Transferring"
  encodeJson Error = fromString "Error"

instance decodeCreepState :: DecodeJson CreepState where
  decodeJson j | toString j == Just "Idle" = Right Idle
               | toString j == Just "Harvesting" = Right Harvesting
               | toString j == Just "Transferring" = Right Transferring
               | toString j == Just "Error" = Right Error
               | otherwise = Left "Failed to parse creepState"

mainLoop :: Eff BaseScreepsEffects Unit
mainLoop = do
  reports <- generateReports
  let reportObservations = analyzeReports reports

  instructionQueue <- getInstructionQueue
  instructionResultObservations <- catMaybes <$> traverse executeInstruction instructionQueue

  state <- getStateFromMemory
  let instructionsAndNextState = (unwrap $ runStateT (StateT (generateInstructions (reportObservations <> instructionResultObservations))) state) :: Tuple (Array Instruction) AiState

  writeStateToMemory $ snd instructionsAndNextState
  writeInstructionsToQueue $ fst instructionsAndNextState

  pure unit

getCreepCapacity :: Creep -> Tuple Int Int
getCreepCapacity creep = Tuple (Creep.totalAmtCarrying creep) (Creep.carryCapacity creep)

generateReports :: Eff BaseScreepsEffects Reports
generateReports = do
  game <- Game.getGameGlobal

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
    }

analyzeReports :: Reports -> Array Observation
analyzeReports (Reports
  { numberOfCreeps: numberOfCreeps
  , creepCapacities: creepCapacities
  , ticksToDowngrade: ticksToDowngrade
  , sourceLocations: sourceLocations
  }) = catMaybes
    [
      if numberOfCreeps < 10 then (Just UnderCreepCap) else Nothing
    ] <> map SourceLocated sourceLocations

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
executeInstruction (HarvestEnergy creepName pt) = pure Nothing

getStateFromMemory :: Eff BaseScreepsEffects AiState
getStateFromMemory = do
  -- HACK UNTIL FULLY MIGRATED
  mem <- Memory.getMemoryGlobal
  creepStatesHack <- (Memory.get mem "creepStates") :: forall e. (EffScreepsCommand e) (Either String (M.StrMap CreepState))
  pure $ either (const $ AiState { creepStates: M.empty }) (\creepStates -> AiState { creepStates: creepStates }) creepStatesHack

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
  respondToObservation (AiState { creepStates: creepStates }) (SourceLocated point) = foldl (instructCreepsToHarvestSource point) { accum: state, value: [] } (M.keys creepStates)

  instructCreepsToHarvestSource :: Point -> Accum AiState (Array Instruction) -> String -> Accum AiState (Array Instruction)
  instructCreepsToHarvestSource pt { accum: (AiState state), value: instructions } creepName | (M.lookup creepName state.creepStates) == Just Error = { accum: (AiState state), value: [] }
                                                                                             | (M.lookup creepName state.creepStates) == Just Idle = instructCreepToHarvest pt { accum: (AiState state), value: instructions } creepName
                                                                                             | (M.lookup creepName state.creepStates) == Just Harvesting = instructCreepToHarvest pt { accum: (AiState state), value: instructions } creepName
                                                                                             | (M.lookup creepName state.creepStates) == Just Transferring = { accum: (AiState state), value: [] }
                                                                                             | otherwise = { accum: (AiState state), value: [] }

  instructCreepToHarvest :: Point -> Accum AiState (Array Instruction) -> String -> Accum AiState (Array Instruction)
  instructCreepToHarvest pt { accum: (AiState state), value: instructions } creepName = { accum: (AiState { creepStates: (M.update (const $ Just Harvesting) creepName state.creepStates) }), value: [HarvestEnergy creepName pt] }


main :: Eff BaseScreepsEffects Unit
main = do
  game <- Game.getGameGlobal

  let creeps = Game.creeps game
  let spawn = M.lookup "Spawn1" $ Game.spawns game

  mainLoop
  creepStates <- (maybe (pure M.empty) (\spawn -> traverse (doCreepAction spawn) creeps) spawn)

  mem <- Memory.getMemoryGlobal
  Memory.set mem "creepStates" (encodeJson creepStates)

  pure unit

doCreepAction :: Spawn -> Creep -> Eff BaseScreepsEffects CreepState
doCreepAction spawn creep = do
  mem <- Memory.getMemoryGlobal
  let creepName = Creep.name creep

  eitherCreepStates <- (Memory.get mem "creepStates") :: forall e. (EffScreepsCommand e) (Either String (M.StrMap CreepState))
  let creepStates = either (const M.empty) id eitherCreepStates
  let creepState = maybe Idle id $ M.lookup creepName creepStates

  log $ "[" <> creepName <> " STATE]: " <> show creepState

  doDecideCreepAction spawn creepState creep

doDecideCreepAction :: Spawn -> CreepState -> Creep -> Eff BaseScreepsEffects CreepState
doDecideCreepAction spawn state creep | state == Error        = pure Idle
                                      | state == Idle         = doCollectEnergy creep
                                      | state == Harvesting   = doDecideFromHarvesting creep
                                      | state == Transferring = doTransferEnergy spawn creep
                                      | otherwise             = pure Idle

doDecideFromHarvesting :: Creep -> Eff BaseScreepsEffects CreepState
doDecideFromHarvesting creep | (Creep.totalAmtCarrying creep) < (Creep.carryCapacity creep) = doCollectEnergy creep
                             | otherwise = pure Transferring

doTransferEnergy :: Spawn -> Creep -> Eff BaseScreepsEffects CreepState
doTransferEnergy spawn creep = doRunCommands $
                                 (flip catchError) handleErrors $ do
                                   doSelectRecipient spawn creep
                                   pure Idle where

    handleErrors :: CommandError -> ExceptT CommandError (Eff BaseScreepsEffects) CreepState
    handleErrors e | e == OutOfRangeError = pure Transferring
                   | e == OutOfEnergyError = pure Idle
                   | e == OutOfResourcesError = pure Idle
                   | otherwise = throwError e

    doSelectRecipient :: Spawn -> Creep -> ExceptT CommandError (Eff BaseScreepsEffects) Unit
    doSelectRecipient spawn creep = case Room.controller $ RoomObject.room creep of
                                        Just controller -> if Controller.ticksToDowngrade controller < 20000
                                                              then do
                                                                doTryCommand "MOVE_CREEP_TO_CONTROLLER" $ Creep.moveTo creep (TargetObj controller)
                                                                doTryCommand "TRANSFER_ENERGY_TO_CONTROLLER" $ Creep.transferToStructure creep controller resource_energy
                                                              else do
                                                                doTryCommand "MOVE_CREEP_TO_SPAWN" $ Creep.moveTo creep (TargetObj spawn)
                                                                doTryCommand "TRANSFER_ENERGY_TO_SPAWN" $ Creep.transferToStructure creep spawn resource_energy
                                        Nothing -> do
                                          doTryCommand "MOVE_CREEP_TO_SPAWN" $ Creep.moveTo creep (TargetObj spawn)
                                          doTryCommand "TRANSFER_ENERGY_TO_SPAWN" $ Creep.transferToStructure creep spawn resource_energy

doRunCommands :: ExceptT CommandError (Eff BaseScreepsEffects) CreepState -> Eff BaseScreepsEffects CreepState
doRunCommands command = either (const $ pure Error) pure =<< runExceptT command

doCollectEnergy :: Creep -> Eff BaseScreepsEffects CreepState
doCollectEnergy creep = do
  let room = RoomObject.room creep
  let sources = Room.find room find_sources

  let source = head sources

  maybe (log "No more sources" >>= (const $ pure Idle)) doHarvestEnergy source where

    doHarvestEnergy :: Source -> Eff BaseScreepsEffects CreepState
    doHarvestEnergy source = doRunCommands $ (flip catchError) handleOutOfRange $ doMoveAndHarvest creep source

    handleOutOfRange :: CommandError -> ExceptT CommandError (Eff BaseScreepsEffects) CreepState
    handleOutOfRange e | e == OutOfRangeError = pure Harvesting
                       | otherwise = throwError e

doMoveAndHarvest :: Creep -> Source -> ExceptT CommandError (Eff BaseScreepsEffects) CreepState
doMoveAndHarvest creep source = do
  doTryCommand "MOVE_CREEP_TO_SOURCE" $ Creep.moveTo creep (TargetObj source)
  doTryCommand "HARVEST_SOURCE" $ Creep.harvestSource creep source
  pure Harvesting

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
