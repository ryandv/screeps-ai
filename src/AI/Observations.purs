module AI.Observations(analyzeReports) where

import Prelude (id, map, otherwise, ($), (<), (<<<), (<>), (==), (>=))

import Data.Array (catMaybes, cons, filter, head, (:))
import Data.Foldable (foldr, foldl)
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap as M
import Data.Tuple (Tuple(..), fst, snd)

import Types (Instruction(..), Observation(..), Point(..), Reports(..), chebyshevDistance)

unfoldStrMap :: forall a. M.StrMap a -> Array (Tuple String a)
unfoldStrMap = M.fold (\acc key val -> (Tuple key val):acc) []

analyzeReports :: Reports -> M.StrMap (Array Observation)
analyzeReports (Reports
  { numberOfCreeps: numberOfCreeps
  , creepCapacities: creepCapacities
  , ticksToDowngrade: ticksToDowngrade
  , sourceLocations: sourceLocations
  , creepLocations: creepLocations
  , processInstructions: processInstructions
  , controllerLocation: controllerLocation
  }) = foldr ($) creepObservations globalObservations where

    creepObservations = M.mapWithKey (\creepName creepCapacity ->
      catMaybes (
        [ if (fst creepCapacity) >= (snd creepCapacity) then Just $ CreepFull creepName else Nothing ]
          <> [ if (fst creepCapacity) == 0 then Just $ CreepEmpty creepName else Nothing ]
          <> [ if isCurrentlyMoving (Tuple creepName <<< maybe [] id $ M.lookup creepName processInstructions) then toArrivedObservation creepLocations (Tuple creepName <<< maybe [] id $ M.lookup creepName processInstructions) else Nothing ]
      )
    ) creepCapacities

    globalObservations :: Array (M.StrMap (Array Observation) -> M.StrMap (Array Observation))
    globalObservations =
      [ (\observations -> foldl (\acc sourceLocation -> map (cons $ (SourceLocated sourceLocation)) acc) observations sourceLocations)
      , (\observations -> if ticksToDowngrade < 20000 then map (cons $ (ControllerIsLow controllerLocation)) observations else observations)
      , (\observations -> if numberOfCreeps < 10 then M.alter (maybe (Just [UnderCreepCap]) (Just <<< cons UnderCreepCap)) "Spawn1" observations else observations)
      ]

-- sourcelocated controllerislow undercreepcap

hasArrivedAtDestination :: M.StrMap Point -> Tuple String (Array Instruction) -> Boolean
hasArrivedAtDestination creepLocations (Tuple creepName instructions) = 1 >= (chebyshevDistance hackCurrentPositionOrOrigin hackDestinationOrOrigin)
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
toArrivedObservation creepLocations processInstructions | hasArrivedAtDestination creepLocations processInstructions = Just <<< Arrived $ fst processInstructions
                                                      | otherwise = Nothing

creepFull :: M.StrMap (Tuple Int Int) -> Array Observation
creepFull creepCapacities = map toObservation <<< filter isFull $ unfoldStrMap creepCapacities where
  isFull (Tuple _ (Tuple carrying capacity)) = carrying >= capacity
  toObservation (Tuple creepName _) = CreepFull creepName

creepEmpty :: M.StrMap (Tuple Int Int) -> Array Observation
creepEmpty creepCapacities = map toObservation <<< filter isEmpty $ unfoldStrMap creepCapacities where
  isEmpty (Tuple creepName (Tuple carrying _)) = carrying == 0
  toObservation (Tuple creepName _) = CreepEmpty creepName
