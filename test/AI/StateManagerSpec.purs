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

observations =
  [ UnderCreepCap
  , SourceLocated $ Point 0 0
  , SourceLocated $ Point 1 1
  , ControllerIsLow $ Point 22 15
  ]

currentState = AiState
    { creepContexts: M.singleton "Alice" $ CreepContext
      { creepStates: Idle
      , creepInstructions: []
      }
    }

spec :: forall r. Spec r Unit
spec = do
  describe "AI.StateManager" $ do
    describe "transfers energy to the Room's Controller" $ do
      it "instructs Idle Creeps to start harvesting energy" do
        creepInstructionsFor "Alice" `shouldContain` (MoveTo "Alice" (Point 0 0))
        creepInstructionsFor "Alice" `shouldContain` (HarvestSource "Alice" (Point 0 0)) where

          instructionsAndNextState = (unwrap $ runStateT (StateT (generateInstructions observations)) currentState) :: Tuple (Array Instruction) AiState

          creepInstructionsFor creepName = maybe [] (\(CreepContext creepContext) -> creepContext.creepInstructions) (M.lookup creepName (unwrap $ snd instructionsAndNextState).creepContexts)
