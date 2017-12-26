module AI.Reports where

import Prelude

import Data.Array (catMaybes, filter, head, (:))
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap as M
import Data.Tuple (Tuple(..), fst, snd)

import Types

analyzeReports :: Reports -> Array Observation
analyzeReports (Reports
  { numberOfCreeps: numberOfCreeps
  , creepCapacities: creepCapacities
  , ticksToDowngrade: ticksToDowngrade
  , sourceLocations: sourceLocations
  , creepLocations: creepLocations
  , creepInstructions: creepInstructions
  , controllerLocation: controllerLocation
  }) = catMaybes
    [ if numberOfCreeps < 10 then (Just UnderCreepCap) else Nothing
    ]
      <> map SourceLocated sourceLocations
      <> catMaybes [ if ticksToDowngrade < 20000 then Just (ControllerIsLow controllerLocation) else Nothing ]
      <> creepFull creepCapacities
      <> creepEmpty creepCapacities
      <> catMaybes (map (toArrivedObservation creepLocations) (filter isCurrentlyMoving unfoldedCreepInstructions)) where

  unfoldedCreepInstructions :: Array (Tuple String (Array Instruction))
  unfoldedCreepInstructions = M.fold (\acc key val -> (Tuple key val):acc) [] creepInstructions

hasArrivedAtDestination :: M.StrMap Point -> Tuple String (Array Instruction) -> Boolean
hasArrivedAtDestination creepLocations (Tuple creepName instructions) = 1 >= chebyshevSquared
  (getCurrentPosition creepLocations creepName)
  (maybe (Point 0 0) (\instruction -> case instruction of
    MoveTo _ destination -> destination
    _ -> Point 0 0) $ head instructions)

getCurrentPosition :: M.StrMap Point -> String -> Point
getCurrentPosition creepLocations creepName = maybe (Point 0 0) id $ M.lookup creepName creepLocations

isCurrentlyMoving :: Tuple String (Array Instruction) -> Boolean
isCurrentlyMoving instructions = case head (snd instructions) of
                                        (Just (MoveTo _ _ )) -> true
                                        _ -> false

toArrivedObservation :: M.StrMap Point -> Tuple String (Array Instruction) -> Maybe Observation
toArrivedObservation creepLocations creepInstructions | hasArrivedAtDestination creepLocations creepInstructions = Just <<< Arrived $ fst creepInstructions
                                                      | otherwise = Nothing

creepFull :: M.StrMap (Tuple Int Int) -> Array Observation
creepFull creepCapacities = map (\(Tuple creepName _) -> CreepFull creepName) $ filter (\(Tuple creepName (Tuple carrying capacity)) -> carrying >= capacity) $ M.fold (\acc key val -> (Tuple key val):acc) [] creepCapacities

creepEmpty :: M.StrMap (Tuple Int Int) -> Array Observation
creepEmpty creepCapacities = map (\(Tuple creepName _) -> CreepEmpty creepName) $ filter (\(Tuple creepName (Tuple carrying _)) -> carrying == 0) $ M.fold (\acc key val -> (Tuple key val):acc) [] creepCapacities
