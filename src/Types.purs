module Types where

import Prelude

import Data.Argonaut.Core (fromNumber, fromObject, fromString, toObject, toString)
import Data.Argonaut.Decode (class DecodeJson, getField)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Ord as Ord
import Data.StrMap as M
import Data.Tuple (Tuple(..))

data Point = Point Int Int

derive instance eqPoint :: Eq Point

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

chebyshevDistance :: Point -> Point -> Int
chebyshevDistance (Point x1 y1) (Point x2 y2) = max (Ord.abs $ x2 - x1) (Ord.abs $ y2 - y1)

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
  show CannotSpawnCreep = "CannotSpawnCreep"
  show (SourceLocated pt) = "SourceLocated " <> show pt
  show (Arrived creepName) = "Arrived " <> creepName
  show (CreepFull creepName) = "CreepFull " <> creepName
  show (CreepEmpty creepName) = "CreepEmpty " <> creepName
  show (ControllerIsLow pt) = "ControllerIsLow " <> show pt

data Instruction = SpawnCreep | DoNothing String | MoveTo String Point | HarvestSource String Point | TransferEnergyTo String Point

derive instance eqInstruction :: Eq Instruction

instance showInstruction :: Show Instruction where
  show SpawnCreep = "SpawnCreep"
  show (DoNothing creepName) = "DoNothing " <> creepName
  show (MoveTo creepName point) = "MoveTo " <> creepName <> " " <> show point
  show (HarvestSource creepName point) = "HarvestSource " <> creepName <> " " <> show point
  show (TransferEnergyTo creepName point) = "TransferEnergyTo " <> creepName <> " " <> show point

instance encodeInstruction :: EncodeJson Instruction where
  encodeJson SpawnCreep = fromObject $ M.fromFoldable
    [ Tuple "instructionType" $ fromString "SpawnCreep"
    , Tuple "payload" $ fromObject $ M.fromFoldable []
    ]
  encodeJson (DoNothing creepName) = fromObject $ M.fromFoldable
    [ Tuple "instructionType" $ fromString "DoNothing"
    , Tuple "payload" $ fromObject $ M.fromFoldable
      [ Tuple "creepName" $ fromString creepName ]
    ]
  encodeJson (MoveTo creepName pt) = fromObject $ M.fromFoldable
    [ Tuple "instructionType" $ fromString "MoveTo"
    , Tuple "payload" $ fromObject $ M.fromFoldable
      [ Tuple "creepName" $ fromString creepName
      , Tuple "destination" $ encodeJson pt
      ]
    ]
  encodeJson (HarvestSource creepName pt) = fromObject $ M.fromFoldable
    [ Tuple "instructionType" $ fromString "HarvestSource"
    , Tuple "payload" $ fromObject $ M.fromFoldable
      [ Tuple "creepName" $ fromString creepName
      , Tuple "sourceLocation" $ encodeJson pt
      ]
    ]
  encodeJson (TransferEnergyTo creepName pt) = fromObject $ M.fromFoldable
    [ Tuple "instructionType" $ fromString "TransferEnergyTo"
    , Tuple "payload" $ fromObject $ M.fromFoldable
      [ Tuple "creepName" $ fromString creepName
      , Tuple "targetLocation" $ encodeJson pt
      ]
    ]

instance decodeInstruction :: DecodeJson Instruction where
  decodeJson instruction  = let instructionType = (getField (maybe (M.fromFoldable []) id (toObject instruction)) "instructionType") in
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

data CreepState = Idle | Moving | Harvesting | Transferring | Error

data AiState = AiState
  { creepContexts :: (M.StrMap CreepContext)
  }

instance newtypeAiState :: Newtype AiState { creepContexts :: (M.StrMap CreepContext) } where
  wrap state = AiState state
  unwrap (AiState state) = state

data CreepContext = CreepContext
  { creepState :: CreepState
  , creepInstructions :: (Array Instruction)
  }

instance newtypeCreepContext :: Newtype CreepContext { creepState :: CreepState , creepInstructions :: (Array Instruction) } where
  wrap ctx = CreepContext ctx
  unwrap (CreepContext ctx) = ctx

getCreepInstructions :: AiState -> M.StrMap (Array Instruction)
getCreepInstructions (AiState state) = map (\(CreepContext context) -> context.creepInstructions) state.creepContexts

instance encodeAiState :: EncodeJson AiState where
  encodeJson (AiState { creepContexts: creepContexts }) = fromObject $ M.fromFoldable
    [ Tuple "creepContexts" $ encodeJson creepContexts
    ]

instance encodeCreepContext :: EncodeJson CreepContext where
  encodeJson (CreepContext { creepState: creepState, creepInstructions: creepInstructions }) = fromObject $ M.fromFoldable
    [ Tuple "creepState" $ encodeJson creepState
    , Tuple "creepInstructions" $ encodeJson creepInstructions
    ]

instance decodeCreepContext :: DecodeJson CreepContext where
  decodeJson json = do
    creepState <- getField (maybe (M.fromFoldable []) id (toObject json)) "creepState"
    creepInstructions <- getField (maybe (M.fromFoldable []) id (toObject json)) "creepInstructions"
    pure $ CreepContext { creepState: creepState, creepInstructions: creepInstructions }

instance decodeAiState :: DecodeJson AiState where
  decodeJson json = do
    creepContexts <- getField (maybe (M.fromFoldable []) id (toObject json)) "creepContexts"
    pure $ AiState { creepContexts: creepContexts }

instance showCreepState :: Show CreepState where
  show Idle = "Idle"
  show Moving = "Moving"
  show Harvesting = "Harvesting"
  show Transferring = "Transferring"
  show Error = "Error"

derive instance eqCreepState :: Eq CreepState

data CommandError = UndistinguishedErrors | OutOfRangeError | OutOfEnergyError | OutOfResourcesError

derive instance eqCommandError :: Eq CommandError

newtype MyIdentity a = MyIdentity (Identity a)

instance newtypeIdentity :: Newtype (MyIdentity a) a where
  wrap a = (MyIdentity (Identity a))
  unwrap (MyIdentity (Identity a)) = a

instance encodeCreepState :: EncodeJson CreepState where
  encodeJson Idle = fromString "Idle"
  encodeJson Moving = fromString "Moving"
  encodeJson Harvesting = fromString "Harvesting"
  encodeJson Transferring = fromString "Transferring"
  encodeJson Error = fromString "Error"

instance decodeCreepState :: DecodeJson CreepState where
  decodeJson j | toString j == Just "Idle" = Right Idle
               | toString j == Just "Moving" = Right Moving
               | toString j == Just "Harvesting" = Right Harvesting
               | toString j == Just "Transferring" = Right Transferring
               | toString j == Just "Error" = Right Error
               | otherwise = Left "Failed to parse creepState"
