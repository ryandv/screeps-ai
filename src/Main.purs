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
import Screeps.Spawn as Spawn

data CreepState = Idle | Harvesting | Transferring | Error

data AiState = AiState

instance encodeAiState :: EncodeJson AiState where
  encodeJson AiState = fromString "AiState"

data Observation = UnderCreepCap | CannotSpawnCreep
data Reports = Reports
  { numberOfCreeps :: Int
  , creepCapacities :: M.StrMap (Tuple Int Int)
  , ticksToDowngrade :: Int
  }

data Instruction = SpawnCreep

instance encodeInstruction :: EncodeJson Instruction where
  encodeJson SpawnCreep = fromString "SpawnCreep"

instance decodeInstruction :: DecodeJson Instruction where
  decodeJson instruction = Right SpawnCreep

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
  let instructionsAndNextState = (unwrap $ runStateT (StateT (generateInstruction (reportObservations <> instructionResultObservations))) state) :: Tuple (Array Instruction) AiState

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

  let creepCapacities = map getCreepCapacity creeps

  pure $ Reports
    { numberOfCreeps: maybe 0 id <<< Int.fromNumber $ M.size creeps
    , creepCapacities: creepCapacities
    , ticksToDowngrade: maybe 0 (\spawn -> case Room.controller $ RoomObject.room spawn of
                                  Just controller -> Controller.ticksToDowngrade controller
                                  Nothing -> 0) spawn
    }

analyzeReports :: Reports -> Array Observation
analyzeReports (Reports
  { numberOfCreeps: numberOfCreeps
  , creepCapacities: creepCapacities
  , ticksToDowngrade: ticksToDowngrade
  }) = catMaybes [
    if numberOfCreeps < 10 then (Just UnderCreepCap) else Nothing
  ]

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

getStateFromMemory :: Eff BaseScreepsEffects AiState
getStateFromMemory = pure $ AiState

writeStateToMemory :: AiState -> Eff BaseScreepsEffects Unit
writeStateToMemory state = do
  mem <- Memory.getMemoryGlobal
  Memory.set mem "aiState" (encodeJson state)

writeInstructionsToQueue :: (Array Instruction) -> Eff BaseScreepsEffects Unit
writeInstructionsToQueue instructions = do
  mem <- Memory.getMemoryGlobal
  Memory.set mem "instructionQueue" (encodeJson instructions)

generateInstruction :: (Array Observation) -> AiState -> MyIdentity (Tuple (Array Instruction) AiState)
generateInstruction observations state = MyIdentity $ Identity $ Tuple (instructionsAndNextState.value) (instructionsAndNextState.accum) where
  instructionsAndNextState = mapAccumL (\state observation -> { accum: state, value: SpawnCreep }) state observations

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
                                        Just controller -> if Controller.ticksToDowngrade controller < 10000
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
  liftEff $ (doLogReturnCode commandName result :: (Eff (cmd :: CMD, console :: CONSOLE, tick :: TICK, time :: TIME, memory :: MEMORY) Unit))
  throwError error

doLogReturnCode :: String -> ReturnCode -> forall e. (EffScreepsCommand e) Unit
doLogReturnCode actionName returnCode = log $ "[FAILED " <> actionName <> "]:" <> show returnCode

energyParts :: Array BodyPartType
energyParts = [part_move, part_work, part_carry]
