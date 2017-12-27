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


creepInstructionsFor :: String -> AiState -> Array Instruction
creepInstructionsFor creepName state = maybe [] (\(CreepContext creepContext) -> creepContext.creepInstructions) (M.lookup creepName (unwrap state).creepContexts)

creepStateFor :: String -> AiState -> Maybe CreepState
creepStateFor creepName state = map (\(CreepContext creepContext) -> creepContext.creepStates) (M.lookup creepName (unwrap state).creepContexts)

spec :: forall r. Spec r Unit
spec = do
  describe "AI.StateManager" $ do
    describe "transfers energy to the Room's Controller" $ do
      it "instructs Idle Creeps to start harvesting energy" $ let
        currentState = AiState
            { creepContexts: M.singleton "Alice" $ CreepContext
              { creepStates: Idle
              , creepInstructions: []
              }
            }
        observations = [ UnderCreepCap , SourceLocated $ Point 0 0 , SourceLocated $ Point 1 1 , ControllerIsLow $ Point 22 15 ]
        instructionsAndNextState = (unwrap $ runStateT (StateT (generateInstructions observations)) currentState) :: Tuple (Array Instruction) AiState in do

          creepStateFor "Alice" (snd instructionsAndNextState) `shouldEqual` (Just Harvesting)
          creepInstructionsFor "Alice" (snd instructionsAndNextState) `shouldEqual` [ MoveTo "Alice" (Point 0 0), HarvestSource "Alice" (Point 0 0) ]

      it "instructs Harvesting Creeps to start harvesting once they have Arrived at their destination" $ let
        currentState = AiState
          { creepContexts: M.singleton "Alice" $ CreepContext
            { creepStates: Harvesting
            , creepInstructions: [ MoveTo "Alice" (Point 0 0), HarvestSource "Alice" (Point 0 0) ]
            }
          }
        observations = [ UnderCreepCap , SourceLocated $ Point 0 0 , SourceLocated $ Point 1 1 , ControllerIsLow $ Point 22 15 , Arrived "Alice" ]
        instructionsAndNextState = (unwrap $ runStateT (StateT (generateInstructions observations)) currentState) :: Tuple (Array Instruction) AiState in do

          creepStateFor "Alice" (snd instructionsAndNextState) `shouldEqual` (Just Harvesting)
          creepInstructionsFor "Alice" (snd instructionsAndNextState) `shouldEqual` [ HarvestSource "Alice" (Point 0 0) ]

      it "instructs Harvesting Creeps to stop harvesting once they are full" $ let
        currentState = AiState
          { creepContexts: M.singleton "Alice" $ CreepContext
            { creepStates: Harvesting
            , creepInstructions: [ HarvestSource "Alice" (Point 0 0) ]
            }
          }
        observations = [ UnderCreepCap , SourceLocated $ Point 0 0 , SourceLocated $ Point 1 1 , ControllerIsLow $ Point 22 15 , CreepFull "Alice" ]
        instructionsAndNextState = (unwrap $ runStateT (StateT (generateInstructions observations)) currentState) :: Tuple (Array Instruction) AiState in do

          creepStateFor "Alice" (snd instructionsAndNextState) `shouldEqual` (Just Transferring)
          creepInstructionsFor "Alice" (snd instructionsAndNextState) `shouldEqual` [ ]

      it "instructs Transferring Creeps to move to the Room Controller" $ let
        currentState = AiState
          { creepContexts: M.singleton "Alice" $ CreepContext
            { creepStates: Transferring
            , creepInstructions: []
            }
          }
        observations = [ UnderCreepCap , SourceLocated $ Point 0 0 , SourceLocated $ Point 1 1 , ControllerIsLow $ Point 22 15 , CreepFull "Alice" ]
        instructionsAndNextState = (unwrap $ runStateT (StateT (generateInstructions observations)) currentState) :: Tuple (Array Instruction) AiState in do

          creepStateFor "Alice" (snd instructionsAndNextState) `shouldEqual` (Just Transferring)
          creepInstructionsFor "Alice" (snd instructionsAndNextState) `shouldEqual` [ MoveTo "Alice" (Point 22 15), TransferEnergyTo "Alice" (Point 22 15) ]

      it "instructs Transferring Creeps that have Arrived at the Room Controller to TransferEnergyTo it" $ let
        currentState = AiState
          { creepContexts: M.singleton "Alice" $ CreepContext
            { creepStates: Transferring
            , creepInstructions: [ MoveTo "Alice" (Point 22 15), TransferEnergyTo "Alice" (Point 22 15) ]
            }
          }
        observations = [ UnderCreepCap , SourceLocated $ Point 0 0 , SourceLocated $ Point 1 1 , ControllerIsLow $ Point 22 15 , CreepFull "Alice", Arrived "Alice" ]
        instructionsAndNextState = (unwrap $ runStateT (StateT (generateInstructions observations)) currentState) :: Tuple (Array Instruction) AiState in do

          creepStateFor "Alice" (snd instructionsAndNextState) `shouldEqual` (Just Transferring)
          creepInstructionsFor "Alice" (snd instructionsAndNextState) `shouldEqual` [ TransferEnergyTo "Alice" (Point 22 15) ]

      it "(legacy) enters an unnecessary intermediate state that does nothing" $ let
        currentState = AiState
          { creepContexts: M.singleton "Alice" $ CreepContext
            { creepStates: Transferring
            , creepInstructions: [ TransferEnergyTo "Alice" (Point 22 15) ]
            }
          }
        observations = [ UnderCreepCap , SourceLocated $ Point 0 0 , SourceLocated $ Point 1 1 , ControllerIsLow $ Point 22 15 , CreepFull "Alice" ]
        instructionsAndNextState = (unwrap $ runStateT (StateT (generateInstructions observations)) currentState) :: Tuple (Array Instruction) AiState in do

          creepStateFor "Alice" (snd instructionsAndNextState) `shouldEqual` (Just Transferring)
          creepInstructionsFor "Alice" (snd instructionsAndNextState) `shouldEqual` [ MoveTo "Alice" (Point 22 15), TransferEnergyTo "Alice" (Point 22 15) ]

      it "instructs Creeps that have transferred all their energy to become Idle" $ let
        currentState = AiState
          { creepContexts: M.singleton "Alice" $ CreepContext
            { creepStates: Error -- hack, should rename to the real Transferring - the others are TransferringEnRoute or something
            , creepInstructions: [ TransferEnergyTo "Alice" (Point 22 15) ]
            }
          }
        observations = [ UnderCreepCap , SourceLocated $ Point 0 0 , SourceLocated $ Point 1 1 , ControllerIsLow $ Point 22 15 , CreepEmpty "Alice" ]
        instructionsAndNextState = (unwrap $ runStateT (StateT (generateInstructions observations)) currentState) :: Tuple (Array Instruction) AiState in do

          creepStateFor "Alice" (snd instructionsAndNextState) `shouldEqual` (Just Idle)
          creepInstructionsFor "Alice" (snd instructionsAndNextState) `shouldEqual` []
