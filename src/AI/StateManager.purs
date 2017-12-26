module AI.StateManager where

import Prelude

import Data.Array (catMaybes, concat, filter, head, tail, (:))
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap as M
import Data.Tuple (Tuple(..), fst, snd)
import Data.Traversable (Accum, foldl, mapAccumL, traverse)

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
respondToObservation (AiState state) (CreepEmpty creepName) =
  { accum: (AiState
    { creepStates: (M.update (const $ Just Idle) creepName state.creepStates)
    , creepInstructions: M.update (\instructions -> case head instructions of
                                  (Just (TransferEnergyTo _ _)) -> tail instructions
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
    { creepStates: (M.update (const $ Just Error) creepName state.creepStates)
    , creepInstructions: (M.update (const $ Just [MoveTo creepName controllerLocation, TransferEnergyTo creepName controllerLocation]) creepName state.creepInstructions)
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
