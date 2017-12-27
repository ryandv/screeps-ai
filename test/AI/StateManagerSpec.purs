module Test.AI.StateManagerSpec where

import Prelude

import Test.Spec
import Test.Spec.Assertions

spec :: forall r. Spec r Unit
spec = do
  describe "AI.StateManager" do
    it "works" do
      (2 + 2) `shouldEqual` 4
