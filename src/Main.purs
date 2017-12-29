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

import Types (getCreepInstructions)
import App.Types (BaseScreepsEffects)

main :: Eff BaseScreepsEffects Unit
main = do
  state <- App.Memory.getStateFromMemory
  reports <- App.Reports.generateReports

  let reportObservations = AI.Observations.analyzeReports reports
  log $ show reportObservations

  let creepInstructionQueue = catMaybes $ map head $ M.fold (\acc key val -> val:acc) [] $ getCreepInstructions state

  instructionResultObservations <- catMaybes <$> traverse App.Execution.executeInstruction creepInstructionQueue

  let nextState = AI.StateManager.generateInstructions (reportObservations <> instructionResultObservations) state
  App.Memory.writeStateToMemory $ nextState

  pure unit
