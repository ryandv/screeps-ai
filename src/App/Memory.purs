module App.Memory where

import Prelude

import Control.Monad.Eff (Eff)

import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either, either)
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap as M
import Data.Traversable (Accum, foldl, mapAccumL, traverse)

import Screeps.Game as Game
import Screeps.Memory as Memory

import App.Types
import Types

getInstructionQueue :: Eff BaseScreepsEffects (Array Instruction)
getInstructionQueue = do
  mem <- Memory.getMemoryGlobal
  instructionQueue <- (Memory.get mem "instructionQueue") :: forall e. (EffScreepsCommand e) (Either String (Array Instruction))
  pure $ either (const []) id instructionQueue


getStateFromMemory :: Eff BaseScreepsEffects AiState
getStateFromMemory = do
  game <- Game.getGameGlobal
  let creeps = Game.creeps game

  mem <- Memory.getMemoryGlobal
  aiState <- (Memory.get mem "aiState") :: forall e. (EffScreepsCommand e) (Either String AiState)

  -- a bit hacky - merge in newly spawned Creeps

  pure $ either (const $ AiState { creepContexts: map (const $ CreepContext { creepStates: Idle , creepInstructions: [] }) creeps })
                (\(AiState state) -> (AiState state
                  { creepContexts = foldl (\acc creepName -> M.alter (maybe (Just $ CreepContext { creepStates: Idle , creepInstructions: [] }) Just) creepName acc) state.creepContexts $ M.keys creeps
                  })) aiState


writeStateToMemory :: AiState -> Eff BaseScreepsEffects Unit
writeStateToMemory state = do
  mem <- Memory.getMemoryGlobal
  Memory.set mem "aiState" (encodeJson state)

writeInstructionsToQueue :: (Array Instruction) -> Eff BaseScreepsEffects Unit
writeInstructionsToQueue instructions = do
  mem <- Memory.getMemoryGlobal
  Memory.set mem "instructionQueue" (encodeJson instructions)
