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

currentState = AiState
    { creepContexts: M.singleton "Alice" $ CreepContext
      { creepStates: Idle
      , creepInstructions: []
      }
    }

creepInstructionsFor :: String -> AiState -> Array Instruction
creepInstructionsFor creepName state = maybe [] (\(CreepContext creepContext) -> creepContext.creepInstructions) (M.lookup creepName (unwrap state).creepContexts)

spec :: forall r. Spec r Unit
spec = do
  describe "AI.StateManager" $ do
    describe "transfers energy to the Room's Controller" $ do
      it "instructs Idle Creeps to start harvesting energy" $ let
        observations = [ UnderCreepCap , SourceLocated $ Point 0 0 , SourceLocated $ Point 1 1 , ControllerIsLow $ Point 22 15 ]
        instructionsAndNextState = (unwrap $ runStateT (StateT (generateInstructions observations)) currentState) :: Tuple (Array Instruction) AiState in do

          creepInstructionsFor "Alice" (snd instructionsAndNextState) `shouldEqual` [ MoveTo "Alice" (Point 0 0), HarvestSource "Alice" (Point 0 0) ]

      it "instructs Harvesting Creeps to start harvesting once they have Arrived at their destination" $ let
        observations = [ UnderCreepCap , SourceLocated $ Point 0 0 , SourceLocated $ Point 1 1 , ControllerIsLow $ Point 22 15 , Arrived "Alice" ]
        instructionsAndNextState = (unwrap $ runStateT (StateT (generateInstructions observations)) currentState) :: Tuple (Array Instruction) AiState in do

          creepInstructionsFor "Alice" (snd instructionsAndNextState) `shouldEqual` [ HarvestSource "Alice" (Point 0 0) ]
