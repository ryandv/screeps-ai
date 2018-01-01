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
generateInstructions observations oldState = AiState $ { creepContexts: M.fold respondToObservations ((unwrap oldState).creepContexts) observations }

respondToObservations :: (M.StrMap ProcessContext) -> String -> (Array Observation) -> (M.StrMap ProcessContext)
respondToObservations oldContexts creepName observations = foldl (respondToObservation creepName) oldContexts observations

respondToObservation :: String -> (M.StrMap ProcessContext) -> Observation -> (M.StrMap ProcessContext)
respondToObservation creepName state CannotSpawnCreep = state

respondToObservation creepName oldContexts UnderCreepCap =
  M.alter (const <<< Just $ ProcessContext
    { processState: Error
    , processInstructions: [ SpawnCreep ]
    }
    ) "Spawn1" oldContexts

respondToObservation creepName contexts Arrived = updateContext contexts creepName Nothing tailOrEmptyList
respondToObservation creepName contexts CreepFull = updateContext contexts creepName (Just Transferring) finishHarvest
respondToObservation creepName oldContexts CreepEmpty =
  M.update (\(ProcessContext oldContext) ->
    case (head (oldContext.processInstructions)) of
      Just (TransferEnergyTo _ _) -> Just $ ProcessContext
        { processState: Idle
        , processInstructions: tailOrEmptyList oldContext.processInstructions
        }
      _ -> Just $ ProcessContext oldContext
    ) creepName oldContexts

respondToObservation creepName contexts (SourceLocated point) = respondToSourceLocated creepName contexts point
respondToObservation creepName contexts (ControllerIsLow controllerLocation) = respondToControllerIsLow creepName contexts controllerLocation

respondToControllerIsLow :: String -> (M.StrMap ProcessContext) -> Point -> (M.StrMap ProcessContext)
respondToControllerIsLow creepName contexts controllerLocation = assignTasks instructCreepToTransferToController Transferring creepName contexts where

  instructCreepToTransferToController :: (M.StrMap ProcessContext) -> String -> (M.StrMap ProcessContext)
  instructCreepToTransferToController oldContexts creepName = updateContext oldContexts creepName (Just Error) (const $ [MoveTo creepName controllerLocation, TransferEnergyTo creepName controllerLocation])

respondToSourceLocated :: String -> (M.StrMap ProcessContext) -> Point -> (M.StrMap ProcessContext)
respondToSourceLocated creepName contexts point = assignTasks instructCreepToHarvestSource Idle creepName contexts where

  instructCreepToHarvestSource :: (M.StrMap ProcessContext) -> String -> (M.StrMap ProcessContext)
  instructCreepToHarvestSource oldContexts creepName = updateContext oldContexts creepName (Just Harvesting) (const $ [MoveTo creepName point, HarvestSource creepName point])

updateContext :: (M.StrMap ProcessContext) -> String -> (Maybe ProcessState) -> (Array Instruction -> Array Instruction) -> (M.StrMap ProcessContext)
updateContext oldContexts creepName newState newInstructionGenerator =
  M.update (\(ProcessContext oldContext) -> Just $ ProcessContext
    { processState: maybe oldContext.processState id $ newState
    , processInstructions: newInstructionGenerator $ oldContext.processInstructions
    }) creepName oldContexts

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

assignTasks :: ((M.StrMap ProcessContext) -> String -> (M.StrMap ProcessContext)) -> ProcessState -> String -> (M.StrMap ProcessContext) -> (M.StrMap ProcessContext)
assignTasks taskAssigner targetedProcessState creepName oldContexts | maybe false (\(ProcessContext processContext) -> processContext.processState == targetedProcessState) (M.lookup creepName oldContexts) = taskAssigner oldContexts creepName
                                                                    | true = oldContexts
