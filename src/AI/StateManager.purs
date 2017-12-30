module AI.StateManager(generateInstructions) where

import Prelude (const, flip, id, ($), (<<<), (==))

import Data.Array (concat, filter, head, tail)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap as M
import Data.Tuple (Tuple(..))
import Data.Traversable (foldl)

import Types (AiState(..), ProcessContext(..), CreepState(..), Instruction(..), Observation(..), Point)

generateInstructions :: M.StrMap (Array Observation) -> AiState -> AiState
generateInstructions observations oldState = M.fold respondToObservations oldState observations

respondToObservations :: AiState -> String -> (Array Observation) -> AiState
respondToObservations oldState creepName observations = foldl respondToObservation oldState observations

respondToObservation :: AiState -> Observation -> AiState
respondToObservation state CannotSpawnCreep = state

respondToObservation (AiState oldState) UnderCreepCap = AiState
  { creepContexts: M.alter (const <<< Just $ ProcessContext
    { creepState: Error
    , creepInstructions: [ SpawnCreep ]
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

updateContext :: AiState -> String -> (Maybe CreepState) -> (Array Instruction -> Array Instruction) -> AiState
updateContext (AiState state) creepName newState newInstructionGenerator = AiState
  { creepContexts: M.update (\(ProcessContext oldContext) -> Just $ ProcessContext
    { creepState: maybe oldContext.creepState id $ newState
    , creepInstructions: newInstructionGenerator $ oldContext.creepInstructions
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

assignTasks :: (AiState -> String -> AiState) -> CreepState -> AiState -> AiState
assignTasks taskAssigner targetedCreepState (AiState state) =
  foldl taskAssigner (AiState state) (targetedCreeps targetedCreepState state.creepContexts)

targetedCreeps :: CreepState -> M.StrMap ProcessContext -> Array String
targetedCreeps targetedCreepState creepContexts = filter (maybe false (isCreepTargeted <<< getCreepState) <<< lookupProcessContext) $ M.keys creepContexts where

  creepNames = M.keys creepContexts
  isCreepTargeted = ((==) targetedCreepState)
  getCreepState (ProcessContext context) = context.creepState
  lookupProcessContext = (flip M.lookup) creepContexts
