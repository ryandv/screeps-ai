module AI.StateManager(generateInstructions) where

import Prelude

import Data.Array (concat, filter, head, tail)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap as M
import Data.Tuple (Tuple(..))
import Data.Traversable (Accum, foldl, mapAccumL)

import Types

generateInstructions :: (Array Observation) -> AiState -> MyIdentity (Tuple (Array Instruction) AiState)
generateInstructions observations state = MyIdentity $ Identity $ Tuple (concat $ instructionsAndNextState.value) (instructionsAndNextState.accum) where

  instructionsAndNextState :: Accum AiState (Array (Array Instruction))
  instructionsAndNextState = mapAccumL respondToObservation state observations

respondToObservation :: AiState -> Observation -> Accum AiState (Array Instruction)
respondToObservation state CannotSpawnCreep = { accum: state, value: [] }
respondToObservation state UnderCreepCap = { accum: state, value: [SpawnCreep] }
respondToObservation state (SourceLocated point) = respondToSourceLocated state point
respondToObservation state (Arrived creepName) = updateContext state creepName Nothing tailOrEmptyList
respondToObservation state (CreepFull creepName) = updateContext state creepName (Just Transferring) finishHarvest
respondToObservation state (CreepEmpty creepName) = updateContext state creepName (Just Idle) finishTransferEnergy

respondToObservation state (ControllerIsLow controllerLocation) = respondToControllerIsLow state controllerLocation

respondToControllerIsLow :: AiState -> Point -> Accum AiState (Array Instruction)
respondToControllerIsLow state controllerLocation = assignTasks instructCreepToTransferToController Transferring state controllerLocation where

  instructCreepToTransferToController :: Point -> Accum AiState (Array Instruction) -> String -> Accum AiState (Array Instruction)
  instructCreepToTransferToController controllerLocation { accum: state } creepName = updateContext state creepName (Just Error) (const $ [MoveTo creepName controllerLocation, TransferEnergyTo creepName controllerLocation])

respondToSourceLocated :: AiState -> Point -> Accum AiState (Array Instruction)
respondToSourceLocated state point = assignTasks instructCreepToHarvestSource Idle state point where

  instructCreepToHarvestSource :: Point -> Accum AiState (Array Instruction) -> String -> Accum AiState (Array Instruction)
  instructCreepToHarvestSource pt { accum: state } creepName = updateContext state creepName (Just Harvesting) (const $ [MoveTo creepName pt, HarvestSource creepName pt])

updateContext :: AiState -> String -> (Maybe CreepState) -> (Array Instruction -> Array Instruction) -> Accum AiState (Array Instruction)
updateContext (AiState state) creepName newState newInstructionGenerator =
  { accum: (AiState
    { creepContexts: M.update (\(CreepContext oldContext) -> Just $ CreepContext
      { creepStates: maybe oldContext.creepStates id $ newState
      , creepInstructions: newInstructionGenerator $ oldContext.creepInstructions
      }) creepName state.creepContexts
    })
  , value: []
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

assignTasks :: (Point -> Accum AiState (Array Instruction) -> String -> Accum AiState (Array Instruction)) -> CreepState -> AiState -> Point -> Accum AiState (Array Instruction)
assignTasks taskAssigner targetedCreepState (AiState state) point = foldl (taskAssigner point) { accum: (AiState state), value: [] } targetedCreeps where
  targetedCreeps = filter (\creepName -> maybe false (((==) targetedCreepState) <<< (\(CreepContext context) -> context.creepStates)) (M.lookup creepName state.creepContexts)) $ M.keys state.creepContexts
