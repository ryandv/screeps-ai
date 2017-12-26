module Types where

import Prelude

import Data.Argonaut.Core (fromNumber, fromObject, fromString, toObject, toString)
import Data.Argonaut.Decode (class DecodeJson, getField)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..), either)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Ord as Ord
import Data.StrMap as M
import Data.Tuple (Tuple(..), fst, snd)

data Point = Point Int Int

instance showPoint :: Show Point where
  show (Point x y) = "Point(" <> show x <> "," <> show y <> ")"

instance encodePoint :: EncodeJson Point where
  encodeJson (Point x y) = fromObject $ M.fromFoldable
    [ Tuple "x" $ fromNumber $ Int.toNumber x
    , Tuple "y" $ fromNumber $ Int.toNumber y
    ]

instance decodePoint :: DecodeJson Point where
  decodeJson point = do
    x <- getField (maybe (M.fromFoldable []) id (toObject point)) "x"
    y <- getField (maybe (M.fromFoldable []) id (toObject point)) "y"
    Right $ Point x y

chebyshevSquared :: Point -> Point -> Int
chebyshevSquared (Point x1 y1) (Point x2 y2) = max (Ord.abs $ x2 - x1) (Ord.abs $ y2 - y1)

data Reports = Reports
  { numberOfCreeps :: Int
  , creepCapacities :: M.StrMap (Tuple Int Int)
  , ticksToDowngrade :: Int
  , sourceLocations :: Array Point
  , creepLocations :: M.StrMap Point
  , creepInstructions :: M.StrMap (Array Instruction)
  , controllerLocation :: Point
  }

data Observation = UnderCreepCap | CannotSpawnCreep | SourceLocated Point | Arrived String | CreepFull String | CreepEmpty String | ControllerIsLow Point

instance showObservation :: Show Observation where
  show UnderCreepCap = "UnderCreepCap"
  show CannotSpawnCreep = "UCannotSpawnCreep"
  show (SourceLocated pt) = "SourceLocated " <> show pt
  show (Arrived creepName) = "Arrived " <> creepName
  show (CreepFull creepName) = "CreepFull " <> creepName
  show (CreepEmpty creepName) = "CreepEmpty " <> creepName
  show (ControllerIsLow pt) = "ControllerIsLow " <> show pt

data Instruction = SpawnCreep | DoNothing String | MoveTo String Point | HarvestSource String Point | TransferEnergyTo String Point

instance encodeInstruction :: EncodeJson Instruction where
  encodeJson SpawnCreep = fromObject $ M.fromFoldable
    [ Tuple "instruction_type" $ fromString "SpawnCreep"
    , Tuple "payload" $ fromObject $ M.fromFoldable []
    ]
  encodeJson (DoNothing creepName) = fromObject $ M.fromFoldable
    [ Tuple "instruction_type" $ fromString "DoNothing"
    , Tuple "payload" $ fromObject $ M.fromFoldable
      [ Tuple "creepName" $ fromString creepName ]
    ]
  encodeJson (MoveTo creepName pt) = fromObject $ M.fromFoldable
    [ Tuple "instruction_type" $ fromString "MoveTo"
    , Tuple "payload" $ fromObject $ M.fromFoldable
      [ Tuple "creepName" $ fromString creepName
      , Tuple "destination" $ encodeJson pt
      ]
    ]
  encodeJson (HarvestSource creepName pt) = fromObject $ M.fromFoldable
    [ Tuple "instruction_type" $ fromString "HarvestSource"
    , Tuple "payload" $ fromObject $ M.fromFoldable
      [ Tuple "creepName" $ fromString creepName
      , Tuple "sourceLocation" $ encodeJson pt
      ]
    ]
  encodeJson (TransferEnergyTo creepName pt) = fromObject $ M.fromFoldable
    [ Tuple "instruction_type" $ fromString "TransferEnergyTo"
    , Tuple "payload" $ fromObject $ M.fromFoldable
      [ Tuple "creepName" $ fromString creepName
      , Tuple "targetLocation" $ encodeJson pt
      ]
    ]

instance decodeInstruction :: DecodeJson Instruction where
  decodeJson instruction  = let instructionType = (getField (maybe (M.fromFoldable []) id (toObject instruction)) "instruction_type") in
                                case instructionType of
                                     (Right "SpawnCreep") -> Right SpawnCreep
                                     (Right "MoveTo") -> do
                                       payload <- getField (maybe (M.fromFoldable []) id (toObject instruction)) "payload"
                                       creepName <- getField payload "creepName"
                                       destination <- getField payload "destination"
                                       Right $ MoveTo creepName destination
                                     (Right "HarvestSource") -> do
                                       payload <- getField (maybe (M.fromFoldable []) id (toObject instruction)) "payload"
                                       creepName <- getField payload "creepName"
                                       sourceLocation <- getField payload "sourceLocation"
                                       Right $ HarvestSource creepName sourceLocation
                                     (Right "TransferEnergyTo") -> do
                                       payload <- getField (maybe (M.fromFoldable []) id (toObject instruction)) "payload"
                                       creepName <- getField payload "creepName"
                                       targetLocation <- getField payload "targetLocation"
                                       Right $ TransferEnergyTo creepName targetLocation
                                     (Right _) -> Right $ DoNothing ""
                                     (Left e) -> Left e
