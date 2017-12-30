module Test.AI.StateManagerSpec where

import AI.StateManager (generateInstructions)

import Prelude

import Control.Monad.State.Trans (StateT(..), runStateT)

import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.StrMap as M
import Data.Tuple (Tuple)

import Test.Spec
import Test.Spec.Assertions

import Types

spec :: forall r. Spec r Unit
spec = do
  describe "AI.StateManager" $ do
    describe "spawns Creeps when below the global cap" $ do
      it "instructs the Spawn to SpawnCreep when below the global cap" $ let
        currentState = singletonState "Spawn1" Error []
        observations = M.singleton "Spawn1" [ UnderCreepCap ]
        nextState = generateInstructions observations currentState in do
          creepStateFor "Spawn1" nextState `shouldEqual` (Just Error)
          creepInstructionsFor "Spawn1" nextState `shouldEqual` [ SpawnCreep ]
    describe "transfers energy to the Room's Controller" $ do
      it "instructs Idle Creeps to start harvesting energy" $ let
        currentState = singletonState "Alice" Idle []
        observations = M.singleton "Alice" [ SourceLocated $ Point 0 0 , SourceLocated $ Point 1 1 , ControllerIsLow $ Point 22 15 ]
        nextState = generateInstructions observations currentState in do

          creepStateFor "Alice" nextState `shouldEqual` (Just Harvesting)
          creepInstructionsFor "Alice" nextState `shouldEqual` [ MoveTo "Alice" (Point 0 0), HarvestSource "Alice" (Point 0 0) ]

      it "instructs Harvesting Creeps to start harvesting once they have Arrived at their destination" $ let
        currentState = singletonState "Alice" Harvesting [ MoveTo "Alice" (Point 0 0), HarvestSource "Alice" (Point 0 0) ]
        observations = M.singleton "Alice" [ SourceLocated $ Point 0 0 , SourceLocated $ Point 1 1 , ControllerIsLow $ Point 22 15 , Arrived "Alice" ]
        nextState = generateInstructions observations currentState in do

          creepStateFor "Alice" nextState `shouldEqual` (Just Harvesting)
          creepInstructionsFor "Alice" nextState `shouldEqual` [ HarvestSource "Alice" (Point 0 0) ]

      it "instructs Harvesting Creeps to stop harvesting once they are full" $ let
        currentState = singletonState "Alice" Harvesting [ HarvestSource "Alice" (Point 0 0) ]
        observations = M.singleton "Alice" [ SourceLocated $ Point 0 0 , SourceLocated $ Point 1 1 , ControllerIsLow $ Point 22 15 , CreepFull "Alice" ]
        nextState = generateInstructions observations currentState in do

          creepStateFor "Alice" nextState `shouldEqual` (Just Transferring)
          creepInstructionsFor "Alice" nextState `shouldEqual` [ ]

      it "instructs Transferring Creeps to move to the Room Controller" $ let
        currentState = singletonState "Alice" Transferring []
        observations = M.singleton "Alice" [ SourceLocated $ Point 0 0 , SourceLocated $ Point 1 1 , ControllerIsLow $ Point 22 15 , CreepFull "Alice" ]
        nextState = generateInstructions observations currentState in do

          creepStateFor "Alice" nextState `shouldEqual` (Just Transferring)
          creepInstructionsFor "Alice" nextState `shouldEqual` [ MoveTo "Alice" (Point 22 15), TransferEnergyTo "Alice" (Point 22 15) ]

      it "instructs Transferring Creeps that have Arrived at the Room Controller to TransferEnergyTo it" $ let
        currentState = singletonState "Alice" Transferring [ MoveTo "Alice" (Point 22 15), TransferEnergyTo "Alice" (Point 22 15) ]
        observations = M.singleton "Alice" [ SourceLocated $ Point 0 0 , SourceLocated $ Point 1 1 , ControllerIsLow $ Point 22 15 , CreepFull "Alice", Arrived "Alice" ]
        nextState = generateInstructions observations currentState in do

          creepStateFor "Alice" nextState `shouldEqual` (Just Transferring)
          creepInstructionsFor "Alice" nextState `shouldEqual` [ TransferEnergyTo "Alice" (Point 22 15) ]

      it "(legacy) enters an unnecessary intermediate state that does nothing" $ let
        currentState = singletonState "Alice" Transferring [ TransferEnergyTo "Alice" (Point 22 15) ]
        observations = M.singleton "Alice" [ SourceLocated $ Point 0 0 , SourceLocated $ Point 1 1 , ControllerIsLow $ Point 22 15 , CreepFull "Alice" ]
        nextState = generateInstructions observations currentState in do

          creepStateFor "Alice" nextState `shouldEqual` (Just Transferring)
          creepInstructionsFor "Alice" nextState `shouldEqual` [ MoveTo "Alice" (Point 22 15), TransferEnergyTo "Alice" (Point 22 15) ]

      it "instructs Creeps that have transferred all their energy to become Idle" $ let
        currentState = singletonState "Alice" Error [ TransferEnergyTo "Alice" (Point 22 15) ] -- hack, should rename to the real Transferring - the others are TransferringEnRoute or something 
        observations = M.singleton "Alice" [ SourceLocated $ Point 0 0 , SourceLocated $ Point 1 1 , ControllerIsLow $ Point 22 15 , CreepEmpty "Alice" ]
        nextState = generateInstructions observations currentState in do

          creepStateFor "Alice" nextState `shouldEqual` (Just Idle)
          creepInstructionsFor "Alice" nextState `shouldEqual` []

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
