module AI.StateManager where

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
respondToObservation (AiState state) (Arrived creepName) =
  { accum: (AiState
    { creepContexts: M.update (\(CreepContext oldContext) -> Just $ CreepContext
      { creepStates: oldContext.creepStates
      , creepInstructions: maybe [] id <<< tail $ oldContext.creepInstructions
      }) creepName state.creepContexts
    })
  , value: []
  }
respondToObservation (AiState state) (CreepFull creepName) =
  { accum: (AiState
    { creepContexts: M.update (\(CreepContext oldContext) -> Just $ CreepContext
      { creepStates: Transferring
      , creepInstructions: case head oldContext.creepInstructions of
                             Just (HarvestSource _ _) -> maybe [] id <<< tail $ oldContext.creepInstructions
                             _ -> oldContext.creepInstructions
      }) creepName state.creepContexts
    })
  , value: []
  }
respondToObservation (AiState state) (CreepEmpty creepName) =
  { accum: (AiState
    { creepContexts: M.update (\(CreepContext oldContext) -> Just $ CreepContext
      { creepStates: Idle
      , creepInstructions: case head oldContext.creepInstructions of
                             (Just (TransferEnergyTo _ _)) -> maybe [] id <<< tail $ oldContext.creepInstructions
                             _ -> oldContext.creepInstructions
      }) creepName state.creepContexts
    })
  , value: []
  }
respondToObservation state (ControllerIsLow controllerLocation) = respondToControllerIsLow state controllerLocation

respondToControllerIsLow :: AiState -> Point -> Accum AiState (Array Instruction)
respondToControllerIsLow (AiState state) controllerLocation = foldl (instructCreepToTransferToController controllerLocation) { accum: (AiState state), value: [] } transferringCreeps where
  transferringCreeps = filter (\creepName -> maybe false (((==) Transferring) <<< (\(CreepContext context) -> context.creepStates)) (M.lookup creepName state.creepContexts)) $ M.keys state.creepContexts

instructCreepToTransferToController :: Point -> Accum AiState (Array Instruction) -> String -> Accum AiState (Array Instruction)
instructCreepToTransferToController controllerLocation { accum: (AiState state), value: instructions } creepName =
  { accum: (AiState
    { creepContexts: M.update (const <<< Just $ CreepContext
      { creepStates: Error
      , creepInstructions: [MoveTo creepName controllerLocation, TransferEnergyTo creepName controllerLocation]
      }) creepName state.creepContexts
    })
  , value: []
  }


respondToSourceLocated :: AiState -> Point -> Accum AiState (Array Instruction)
respondToSourceLocated (AiState state) point = foldl (instructCreepToHarvestSource point) { accum: (AiState state), value: [] } idleCreeps where
  idleCreeps = filter (\creepName -> maybe false (((==) Idle) <<< (\(CreepContext context) -> context.creepStates)) (M.lookup creepName state.creepContexts)) $ M.keys state.creepContexts

instructCreepToHarvestSource :: Point -> Accum AiState (Array Instruction) -> String -> Accum AiState (Array Instruction)
instructCreepToHarvestSource pt { accum: (AiState state), value: instructions } creepName =
  { accum: (AiState
    { creepContexts: M.update (const <<< Just $ CreepContext
      { creepStates: Harvesting
      , creepInstructions: [MoveTo creepName pt, HarvestSource creepName pt]
      }) creepName state.creepContexts
    })
  , value: []
  }
