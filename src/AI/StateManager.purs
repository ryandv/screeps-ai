module AI.StateManager(generateInstructions) where

import Prelude (const, flip, id, ($), (<<<), (==))

import Data.Array (concat, filter, head, tail)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap as M
import Data.Tuple (Tuple(..))
import Data.Traversable (foldl)

import Types (AiState(..), ProcessContext(..), ProcessState(..), Instruction(..), Observation(..), Point)

generateInstructions :: M.StrMap (Array Observation) -> AiState -> AiState
generateInstructions observations oldState = M.fold respondToObservations oldState observations

respondToObservations :: AiState -> String -> (Array Observation) -> AiState
respondToObservations oldState creepName observations = foldl respondToObservation oldState observations

respondToObservation :: AiState -> Observation -> AiState
respondToObservation state CannotSpawnCreep = state

respondToObservation (AiState oldState) UnderCreepCap = AiState
  { creepContexts: M.alter (const <<< Just $ ProcessContext
    { processState: Error
    , processInstructions: [ SpawnCreep ]
    }) "Spawn1" oldState.creepContexts
  }

respondToObservation state (Arrived creepName) = updateContext state creepName Nothing tailOrEmptyList
respondToObservation state (CreepFull creepName) = updateContext state creepName (Just Transferring) finishHarvest
respondToObservation state (CreepEmpty creepName) = updateContext state creepName (Just Idle) finishTransferEnergy

respondToObservation state (SourceLocated point) = respondToSourceLocated state point
respondToObservation state (ControllerIsLow controllerLocation) = respondToControllerIsLow state controllerLocation

respondToControllerIsLow :: AiState -> Point -> AiState
respondToControllerIsLow aistate controllerLocation = assignTasks instructCreepToTransferToController Transferring aistate where

  instructCreepToTransferToController :: AiState -> String -> AiState
  instructCreepToTransferToController oldState creepName = updateContext oldState creepName (Just Error) (const $ [MoveTo creepName controllerLocation, TransferEnergyTo creepName controllerLocation])

respondToSourceLocated :: AiState -> Point -> AiState
respondToSourceLocated aistate point = assignTasks instructCreepToHarvestSource Idle aistate where

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

assignTasks :: (AiState -> String -> AiState) -> ProcessState -> AiState -> AiState
assignTasks taskAssigner targetedProcessState (AiState state) =
  foldl taskAssigner (AiState state) (targetedCreeps targetedProcessState state.creepContexts)

targetedCreeps :: ProcessState -> M.StrMap ProcessContext -> Array String
targetedCreeps targetedProcessState creepContexts = filter (maybe false (isCreepTargeted <<< getProcessState) <<< lookupProcessContext) $ M.keys creepContexts where

  creepNames = M.keys creepContexts
  isCreepTargeted = ((==) targetedProcessState)
  getProcessState (ProcessContext context) = context.processState
  lookupProcessContext = (flip M.lookup) creepContexts
