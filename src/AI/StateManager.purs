module AI.StateManager(generateInstructions) where

import Prelude (const, flip, id, ($), (<<<), (==))

import Data.Array (concat, filter, head, tail)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap as M
import Data.Tuple (Tuple(..))
import Data.Traversable (Accum, foldl, mapAccumL)

import Types (AiState(..), CreepContext(..), CreepState(..), Instruction(..), MyIdentity(..), Observation(..), Point)

generateInstructions :: (Array Observation) -> AiState -> MyIdentity (Tuple (Array Instruction) AiState)
generateInstructions observations state = MyIdentity $ Identity $ Tuple (concat $ instructionsAndNextState.value) (instructionsAndNextState.accum) where

  instructionsAndNextState :: Accum AiState (Array (Array Instruction))
  instructionsAndNextState = mapAccumL respondToObservation state observations

respondToObservation :: AiState -> Observation -> Accum AiState (Array Instruction)
respondToObservation state CannotSpawnCreep = { accum: state, value: [] }
respondToObservation state UnderCreepCap = { accum: state, value: [SpawnCreep] }

respondToObservation state (Arrived creepName) = updateContext state creepName Nothing tailOrEmptyList
respondToObservation state (CreepFull creepName) = updateContext state creepName (Just Transferring) finishHarvest
respondToObservation state (CreepEmpty creepName) = updateContext state creepName (Just Idle) finishTransferEnergy

respondToObservation state (SourceLocated point) = respondToSourceLocated state point
respondToObservation state (ControllerIsLow controllerLocation) = respondToControllerIsLow state controllerLocation

respondToControllerIsLow :: AiState -> Point -> Accum AiState (Array Instruction)
respondToControllerIsLow aistate controllerLocation = assignTasks instructCreepToTransferToController Transferring aistate where

  instructCreepToTransferToController :: Accum AiState (Array Instruction) -> String -> Accum AiState (Array Instruction)
  instructCreepToTransferToController { accum: state } creepName = updateContext state creepName (Just Error) (const $ [MoveTo creepName controllerLocation, TransferEnergyTo creepName controllerLocation])

respondToSourceLocated :: AiState -> Point -> Accum AiState (Array Instruction)
respondToSourceLocated aistate point = assignTasks instructCreepToHarvestSource Idle aistate where

  instructCreepToHarvestSource :: Accum AiState (Array Instruction) -> String -> Accum AiState (Array Instruction)
  instructCreepToHarvestSource { accum: state } creepName = updateContext state creepName (Just Harvesting) (const $ [MoveTo creepName point, HarvestSource creepName point])

updateContext :: AiState -> String -> (Maybe CreepState) -> (Array Instruction -> Array Instruction) -> Accum AiState (Array Instruction)
updateContext (AiState state) creepName newState newInstructionGenerator =
  { accum: (AiState
    { creepContexts: M.update (\(CreepContext oldContext) -> Just $ CreepContext
      { creepState: maybe oldContext.creepState id $ newState
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

assignTasks :: (Accum AiState (Array Instruction) -> String -> Accum AiState (Array Instruction)) -> CreepState -> AiState -> Accum AiState (Array Instruction)
assignTasks taskAssigner targetedCreepState (AiState state) =
  foldl taskAssigner
        { accum: (AiState state), value: [] }
        (targetedCreeps targetedCreepState state.creepContexts)

targetedCreeps :: CreepState -> M.StrMap CreepContext -> Array String
targetedCreeps targetedCreepState creepContexts = filter (maybe false (isCreepTargeted <<< getCreepState) <<< lookupCreepContext) $ M.keys creepContexts where

  creepNames = M.keys creepContexts
  isCreepTargeted = ((==) targetedCreepState)
  getCreepState (CreepContext context) = context.creepState
  lookupCreepContext = (flip M.lookup) creepContexts
