module Main where

import Prelude (Unit, bind, discard, map, pure, show, unit, ($), (<$>), (<>))

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Control.Monad.State.Trans (StateT(..), runStateT)

import Data.Array (catMaybes, head, (:))
import Data.Newtype (unwrap)
import Data.StrMap as M
import Data.Tuple (Tuple, fst, snd)
import Data.Traversable (traverse)

import App.Execution as App.Execution
import App.Memory as App.Memory
import App.Reports as App.Reports

import AI.Observations as AI.Observations
import AI.StateManager as AI.StateManager

import Types (AiState, Instruction, getCreepInstructions)
import App.Types (BaseScreepsEffects)

main :: Eff BaseScreepsEffects Unit
main = do
  reports <- App.Reports.generateReports
  let reportObservations = AI.Observations.analyzeReports reports
  log $ show reportObservations

  state <- App.Memory.getStateFromMemory

  instructionQueue <- App.Memory.getInstructionQueue
  let creepInstructionQueue = catMaybes $ map head $ M.fold (\acc key val -> val:acc) [] $ getCreepInstructions state
  instructionResultObservations <- catMaybes <$> traverse App.Execution.executeInstruction (instructionQueue <> creepInstructionQueue)

  let instructionsAndNextState = (unwrap $ runStateT (StateT (AI.StateManager.generateInstructions (reportObservations <> instructionResultObservations))) state) :: Tuple (Array Instruction) AiState

  App.Memory.writeStateToMemory $ snd instructionsAndNextState
  App.Memory.writeInstructionsToQueue $ fst instructionsAndNextState

  pure unit
