module App.Memory where

import Prelude

import Control.Monad.Eff (Eff)

import Data.Argonaut.Encode (encodeJson)
import Data.Array (cons, elem)
import Data.Either (Either, either)
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap as M
import Data.Traversable (Accum, foldl, mapAccumL, traverse)

import Screeps.Game as Game
import Screeps.Memory as Memory

import App.Types
import Types

mergeNewlySpawnedCreep :: M.StrMap ProcessContext -> String -> M.StrMap ProcessContext
mergeNewlySpawnedCreep creepContexts creepName = M.alter (maybe (Just $ ProcessContext { processState: Idle , creepInstructions: [] }) Just) creepName creepContexts

pruneDeceasedCreep :: Array String -> M.StrMap ProcessContext -> String -> M.StrMap ProcessContext
pruneDeceasedCreep creeps creepContexts creepName = M.alter (maybe Nothing (\creepContext -> if creepName `elem` (cons "Spawn1" creeps) then Just creepContext else Nothing)) creepName creepContexts

getStateFromMemory :: Eff BaseScreepsEffects AiState
getStateFromMemory = do
  game <- Game.getGameGlobal
  let creeps = Game.creeps game

  mem <- Memory.getMemoryGlobal
  aiState <- (Memory.get mem "aiState") :: forall e. (EffScreepsCommand e) (Either String AiState)

  -- a bit hacky - merge in newly spawned Creeps, prune deceased ones

  pure $ either (const $ AiState { creepContexts: map (const $ ProcessContext { processState: Idle , creepInstructions: [] }) creeps })
                (\(AiState state) -> (AiState state
                  { creepContexts = foldl (pruneDeceasedCreep (M.keys creeps)) (foldl mergeNewlySpawnedCreep state.creepContexts $ (M.keys creeps)) $ M.keys state.creepContexts
                  })) aiState


writeStateToMemory :: AiState -> Eff BaseScreepsEffects Unit
writeStateToMemory state = do
  mem <- Memory.getMemoryGlobal
  Memory.set mem "aiState" (encodeJson state)
