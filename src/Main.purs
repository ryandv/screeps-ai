module Main where

import Prelude (Unit, bind, discard, id, map, pure, show, unit, ($), (<$>), (<>), (<<<))

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Control.Monad.State.Trans (StateT(..), runStateT)

import Data.Array (catMaybes, cons, head, (:))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.StrMap as M
import Data.Tuple (Tuple, fst, snd)
import Data.Traversable (traverse)

import App.Execution as App.Execution
import App.Memory as App.Memory
import App.Reports as App.Reports

import AI.Observations as AI.Observations
import AI.StateManager as AI.StateManager

import Types (getCreepInstructions, Instruction)
import App.Types (BaseScreepsEffects)

main :: Eff BaseScreepsEffects Unit
main = do
  state <- App.Memory.getStateFromMemory
  reports <- App.Reports.generateReports

  let reportObservations = AI.Observations.analyzeReports reports
  log $ show reportObservations

  let instructions = M.fold (\acc creepName maybeInstruction -> maybe acc (\instruction -> M.insert creepName instruction acc) maybeInstruction) M.empty $ (map head $ getCreepInstructions state)

  instructionResultObservations <- traverse App.Execution.executeInstruction instructions

  let allObservations = M.fold (\acc creepName maybeObservation -> maybe acc (\observation -> M.alter (maybe (Just [observation]) (Just <<< cons observation)) creepName acc) maybeObservation) reportObservations instructionResultObservations

  let nextState = AI.StateManager.generateInstructions allObservations state
  App.Memory.writeStateToMemory $ nextState

  pure unit
