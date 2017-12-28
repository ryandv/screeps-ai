module Test.AI.StateManagerSpec where

import AI.StateManager (generateInstructions)

import Prelude

import Control.Monad.State.Trans (StateT(..), runStateT)

import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.StrMap as M
import Data.Tuple (Tuple, fst, snd)

import Test.Spec
import Test.Spec.Assertions

import Types

spec :: forall r. Spec r Unit
spec = do
  describe "AI.StateManager" $ do
    describe "spawns Creeps when below the global cap" $ do
      it "instructs the Spawn to SpawnCreep when below the global cap" $ let
        currentState = singletonState "Spawn1" Error []
        observations = [ UnderCreepCap ]
        instructionsAndNextState = runStateMachine currentState observations in do
          creepStateFor "Spawn1" (snd instructionsAndNextState) `shouldEqual` (Just Error)
          creepInstructionsFor "Spawn1" (snd instructionsAndNextState) `shouldEqual` [ SpawnCreep ]
    describe "transfers energy to the Room's Controller" $ do
      it "instructs Idle Creeps to start harvesting energy" $ let
        currentState = singletonState "Alice" Idle []
        observations = [ SourceLocated $ Point 0 0 , SourceLocated $ Point 1 1 , ControllerIsLow $ Point 22 15 ]
        instructionsAndNextState = runStateMachine currentState observations in do

          creepStateFor "Alice" (snd instructionsAndNextState) `shouldEqual` (Just Harvesting)
          creepInstructionsFor "Alice" (snd instructionsAndNextState) `shouldEqual` [ MoveTo "Alice" (Point 0 0), HarvestSource "Alice" (Point 0 0) ]

      it "instructs Harvesting Creeps to start harvesting once they have Arrived at their destination" $ let
        currentState = singletonState "Alice" Harvesting [ MoveTo "Alice" (Point 0 0), HarvestSource "Alice" (Point 0 0) ]
        observations = [ SourceLocated $ Point 0 0 , SourceLocated $ Point 1 1 , ControllerIsLow $ Point 22 15 , Arrived "Alice" ]
        instructionsAndNextState = runStateMachine currentState observations in do

          creepStateFor "Alice" (snd instructionsAndNextState) `shouldEqual` (Just Harvesting)
          creepInstructionsFor "Alice" (snd instructionsAndNextState) `shouldEqual` [ HarvestSource "Alice" (Point 0 0) ]

      it "instructs Harvesting Creeps to stop harvesting once they are full" $ let
        currentState = singletonState "Alice" Harvesting [ HarvestSource "Alice" (Point 0 0) ]
        observations = [ SourceLocated $ Point 0 0 , SourceLocated $ Point 1 1 , ControllerIsLow $ Point 22 15 , CreepFull "Alice" ]
        instructionsAndNextState = runStateMachine currentState observations in do

          creepStateFor "Alice" (snd instructionsAndNextState) `shouldEqual` (Just Transferring)
          creepInstructionsFor "Alice" (snd instructionsAndNextState) `shouldEqual` [ ]

      it "instructs Transferring Creeps to move to the Room Controller" $ let
        currentState = singletonState "Alice" Transferring []
        observations = [ SourceLocated $ Point 0 0 , SourceLocated $ Point 1 1 , ControllerIsLow $ Point 22 15 , CreepFull "Alice" ]
        instructionsAndNextState = runStateMachine currentState observations in do

          creepStateFor "Alice" (snd instructionsAndNextState) `shouldEqual` (Just Transferring)
          creepInstructionsFor "Alice" (snd instructionsAndNextState) `shouldEqual` [ MoveTo "Alice" (Point 22 15), TransferEnergyTo "Alice" (Point 22 15) ]

      it "instructs Transferring Creeps that have Arrived at the Room Controller to TransferEnergyTo it" $ let
        currentState = singletonState "Alice" Transferring [ MoveTo "Alice" (Point 22 15), TransferEnergyTo "Alice" (Point 22 15) ]
        observations = [ SourceLocated $ Point 0 0 , SourceLocated $ Point 1 1 , ControllerIsLow $ Point 22 15 , CreepFull "Alice", Arrived "Alice" ]
        instructionsAndNextState = runStateMachine currentState observations in do

          creepStateFor "Alice" (snd instructionsAndNextState) `shouldEqual` (Just Transferring)
          creepInstructionsFor "Alice" (snd instructionsAndNextState) `shouldEqual` [ TransferEnergyTo "Alice" (Point 22 15) ]

      it "(legacy) enters an unnecessary intermediate state that does nothing" $ let
        currentState = singletonState "Alice" Transferring [ TransferEnergyTo "Alice" (Point 22 15) ]
        observations = [ SourceLocated $ Point 0 0 , SourceLocated $ Point 1 1 , ControllerIsLow $ Point 22 15 , CreepFull "Alice" ]
        instructionsAndNextState = runStateMachine currentState observations in do

          creepStateFor "Alice" (snd instructionsAndNextState) `shouldEqual` (Just Transferring)
          creepInstructionsFor "Alice" (snd instructionsAndNextState) `shouldEqual` [ MoveTo "Alice" (Point 22 15), TransferEnergyTo "Alice" (Point 22 15) ]

      it "instructs Creeps that have transferred all their energy to become Idle" $ let
        currentState = singletonState "Alice" Error [ TransferEnergyTo "Alice" (Point 22 15) ] -- hack, should rename to the real Transferring - the others are TransferringEnRoute or something 
        observations = [ SourceLocated $ Point 0 0 , SourceLocated $ Point 1 1 , ControllerIsLow $ Point 22 15 , CreepEmpty "Alice" ]
        instructionsAndNextState = runStateMachine currentState observations in do

          creepStateFor "Alice" (snd instructionsAndNextState) `shouldEqual` (Just Idle)
          creepInstructionsFor "Alice" (snd instructionsAndNextState) `shouldEqual` []

creepInstructionsFor :: String -> AiState -> Array Instruction
creepInstructionsFor creepName state = maybe [] (\(CreepContext creepContext) -> creepContext.creepInstructions) (M.lookup creepName (unwrap state).creepContexts)

creepStateFor :: String -> AiState -> Maybe CreepState
creepStateFor creepName state = map (\(CreepContext creepContext) -> creepContext.creepState) (M.lookup creepName (unwrap state).creepContexts)

singletonState :: String -> CreepState -> Array Instruction -> AiState
singletonState creepName creepState creepInstructions = AiState
  { creepContexts: M.singleton creepName $ CreepContext
    { creepState: creepState
    , creepInstructions: creepInstructions
    }
  }

runStateMachine :: AiState -> Array Observation -> Tuple (Array Instruction) AiState
runStateMachine currentState observations = unwrap $ runStateT (StateT (generateInstructions observations)) currentState
