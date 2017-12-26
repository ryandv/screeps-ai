module AI.Observations(analyzeReports) where

import Prelude

import Data.Array (catMaybes, filter, head, (:))
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap as M
import Data.Tuple (Tuple(..), fst, snd)

import Types

unfoldStrMap :: forall a. M.StrMap a -> Array (Tuple String a)
unfoldStrMap = M.fold (\acc key val -> (Tuple key val):acc) []

analyzeReports :: Reports -> Array Observation
analyzeReports (Reports
  { numberOfCreeps: numberOfCreeps
  , creepCapacities: creepCapacities
  , ticksToDowngrade: ticksToDowngrade
  , sourceLocations: sourceLocations
  , creepLocations: creepLocations
  , creepInstructions: creepInstructions
  , controllerLocation: controllerLocation
  }) = catMaybes (
    [ if numberOfCreeps < 10 then (Just UnderCreepCap) else Nothing ]
      <> map (Just <<< SourceLocated) sourceLocations
      <> [ if ticksToDowngrade < 20000 then Just (ControllerIsLow controllerLocation) else Nothing ]
      <> (map Just $ creepFull creepCapacities)
      <> (map Just $ creepEmpty creepCapacities)
      <> (map (toArrivedObservation creepLocations) (filter isCurrentlyMoving $ unfoldStrMap creepInstructions)))

hasArrivedAtDestination :: M.StrMap Point -> Tuple String (Array Instruction) -> Boolean
hasArrivedAtDestination creepLocations (Tuple creepName instructions) = 1 >= (chebyshevSquared hackCurrentPositionOrOrigin hackDestinationOrOrigin)
    where
      hackCurrentPositionOrOrigin = maybe origin id $ getCurrentPosition creepLocations creepName
      hackDestinationOrOrigin = (maybe origin destinationOrOrigin $ head instructions)
      origin = Point 0 0
      destinationOrOrigin instruction = case instruction of
                                                 MoveTo _ destination -> destination
                                                 _ -> origin

getCurrentPosition :: M.StrMap Point -> String -> Maybe Point
getCurrentPosition creepLocations creepName = M.lookup creepName creepLocations

isCurrentlyMoving :: Tuple String (Array Instruction) -> Boolean
isCurrentlyMoving instructions = case head (snd instructions) of
                                        (Just (MoveTo _ _ )) -> true
                                        _ -> false

toArrivedObservation :: M.StrMap Point -> Tuple String (Array Instruction) -> Maybe Observation
toArrivedObservation creepLocations creepInstructions | hasArrivedAtDestination creepLocations creepInstructions = Just <<< Arrived $ fst creepInstructions
                                                      | otherwise = Nothing

creepFull :: M.StrMap (Tuple Int Int) -> Array Observation
creepFull creepCapacities = map toObservation <<< filter isFull $ unfoldStrMap creepCapacities where
  isFull (Tuple _ (Tuple carrying capacity)) = carrying >= capacity
  toObservation (Tuple creepName _) = CreepFull creepName

creepEmpty :: M.StrMap (Tuple Int Int) -> Array Observation
creepEmpty creepCapacities = map toObservation <<< filter isEmpty $ unfoldStrMap creepCapacities where
  isEmpty (Tuple creepName (Tuple carrying _)) = carrying == 0
  toObservation (Tuple creepName _) = CreepEmpty creepName
