module AI.StateManager(generateInstructions) where

import Prelude (const, flip, id, ($), (<<<), (==))

import Data.Array (concat, filter, head, tail)
import Data.Identity (Identity(..))
import Data.Newtype (unwrap)
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap as M
import Data.Tuple (Tuple(..))
import Data.Traversable (foldl)

import Types (AiState(..), ProcessContext(..), ProcessState(..), Instruction(..), Observation(..), Point)

generateInstructions :: M.StrMap (Array Observation) -> AiState -> AiState
generateInstructions observations oldState = M.fold respondToObservations oldState observations

respondToObservations :: AiState -> String -> (Array Observation) -> AiState
respondToObservations oldState creepName observations = foldl (respondToObservation creepName) oldState observations

respondToObservation :: String -> AiState -> Observation -> AiState
respondToObservation creepName state CannotSpawnCreep = state

respondToObservation creepName (AiState oldState) UnderCreepCap = AiState
  { creepContexts: M.alter (const <<< Just $ ProcessContext
    { processState: Error
    , processInstructions: [ SpawnCreep ]
    }) "Spawn1" oldState.creepContexts
  }

respondToObservation creepName state Arrived = updateContext state creepName Nothing tailOrEmptyList
respondToObservation creepName state CreepFull = updateContext state creepName (Just Transferring) finishHarvest
respondToObservation creepName (AiState state) CreepEmpty = AiState $
  { creepContexts: M.update (\(ProcessContext oldContext) -> case (head (oldContext.processInstructions)) of
                                                                  Just (TransferEnergyTo _ _) -> Just $ ProcessContext
                                                                    { processState: Idle
                                                                    , processInstructions: tailOrEmptyList oldContext.processInstructions
                                                                    }
                                                                  _ -> Just $ ProcessContext oldContext
                                                                    ) creepName state.creepContexts
  }

respondToObservation creepName state (SourceLocated point) = respondToSourceLocated creepName state point
respondToObservation creepName state (ControllerIsLow controllerLocation) = respondToControllerIsLow creepName state controllerLocation

respondToControllerIsLow :: String -> AiState -> Point -> AiState
respondToControllerIsLow creepName aistate controllerLocation = assignTasks instructCreepToTransferToController Transferring creepName aistate where

  instructCreepToTransferToController :: AiState -> String -> AiState
  instructCreepToTransferToController oldState creepName = updateContext oldState creepName (Just Error) (const $ [MoveTo creepName controllerLocation, TransferEnergyTo creepName controllerLocation])

respondToSourceLocated :: String -> AiState -> Point -> AiState
respondToSourceLocated creepName aistate point = assignTasks instructCreepToHarvestSource Idle creepName aistate where

  instructCreepToHarvestSource :: AiState -> String -> AiState
  instructCreepToHarvestSource oldState creepName = updateContext oldState creepName (Just Harvesting) (const $ [MoveTo creepName point, HarvestSource creepName point])

updateContext :: AiState -> String -> (Maybe ProcessState) -> (Array Instruction -> Array Instruction) -> AiState
updateContext (AiState state) creepName newState newInstructionGenerator = AiState
  { creepContexts: M.update (\(ProcessContext oldContext) -> Just $ ProcessContext
    { processState: maybe oldContext.processState id $ newState
    , processInstructions: newInstructionGenerator $ oldContext.processInstructions
    }) creepName state.creepContexts
  }

tailOrEmptyList :: forall a. Array a -> Array a
tailOrEmptyList = maybe [] id <<< tail

finishHarvest :: Array Instruction -> Array Instruction
finishHarvest instructions = case head instructions of
                                    Just (HarvestSource _ _) -> tailOrEmptyList $ instructions
                                    _ -> instructions

finishTransferEnergy :: Array Instruction -> Array Instruction
finishTransferEnergy instructions = case head instructions of
                                    Just (TransferEnergyTo _ _) -> tailOrEmptyList $ instructions
                                    _ -> instructions

assignTasks :: (AiState -> String -> AiState) -> ProcessState -> String -> AiState -> AiState
assignTasks taskAssigner targetedProcessState creepName state | maybe false (\(ProcessContext processContext) -> processContext.processState == targetedProcessState) (M.lookup creepName (unwrap state).creepContexts) = taskAssigner state creepName
                                                              | true = state
