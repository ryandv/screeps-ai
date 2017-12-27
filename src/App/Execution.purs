module App.Execution where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Except.Trans (ExceptT, runExceptT, throwError)

import Data.Array (head)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap as M

import Screeps (BodyPartType, ReturnCode, TargetPosition(..))
import Screeps.Constants (look_sources, ok, part_carry, part_move, part_work, resource_energy)
import Screeps.Creep as Creep
import Screeps.Game as Game
import Screeps.Room as Room
import Screeps.RoomObject as RoomObject
import Screeps.RoomPosition as RoomPosition
import Screeps.Spawn as Spawn

import App.Types
import Types

executeInstruction :: Instruction -> Eff BaseScreepsEffects (Maybe Observation)
executeInstruction (DoNothing creepName) = pure Nothing
executeInstruction SpawnCreep = do
  game <- Game.getGameGlobal

  let maybeSpawn = M.lookup "Spawn1" $ Game.spawns game

  maybe (pure $ Just CannotSpawnCreep) (\spawn -> do
        createCreepResult <- Spawn.createCreep spawn energyParts
        pure $ either (const $ Just CannotSpawnCreep) (const Nothing) createCreepResult) maybeSpawn
executeInstruction (MoveTo creepName (Point x y)) = do
  log $ "Executing MoveTo(" <> creepName <> "," <> show x <> "," <> show y <> ")"
  game <- Game.getGameGlobal
  observation <- runExceptT do
    let maybeCreep = M.lookup creepName $ Game.creeps game

    maybe (throwError UndistinguishedErrors) (\creep -> do
      doTryCommand "MOVE_CREEP" $ Creep.moveTo creep (TargetPt x y)
      pure $ Nothing) maybeCreep

  pure $ either (const Nothing) id $ observation
executeInstruction (HarvestSource creepName (Point x y)) = do
  log $ "Executing HarvestSource(" <> creepName <> "," <> show x <> "," <> show y <> ")"
  game <- Game.getGameGlobal
  observation <- runExceptT do
    let maybeCreep = M.lookup creepName $ Game.creeps game

    maybe (throwError UndistinguishedErrors) (\creep -> do
      let eitherSource = RoomPosition.lookFor (RoomPosition.mkRoomPosition x y (Room.name (RoomObject.room creep))) look_sources

      case eitherSource of
           (Right sources) -> maybe (pure $ Nothing) (\source -> do
             doTryCommand "HARVEST_SOURCE" $ Creep.harvestSource creep source
             pure Nothing) $ head sources
           _ -> pure $ Nothing

      ) maybeCreep

  pure $ either (const Nothing) id $ observation
executeInstruction (TransferEnergyTo creepName (Point x y)) = do
  log $ "Executing TransferEnergyTo(" <> creepName <> "," <> show x <> "," <> show y <> ")"
  game <- Game.getGameGlobal
  observation <- runExceptT do
    let maybeCreep = M.lookup creepName $ Game.creeps game

    maybe (throwError UndistinguishedErrors) (\creep ->
      case Room.controller (RoomObject.room creep) of
        Just controller -> do
          doTryCommand "TRANSFER_ENERGY_TO_CONTROLLER" $ Creep.transferToStructure creep controller resource_energy
          pure Nothing
        _ -> pure $ Nothing) maybeCreep

  pure $ either (const Nothing) id $ observation

energyParts :: Array BodyPartType
energyParts = [part_move, part_work, part_carry]

doTryCommand :: String -> Eff BaseScreepsEffects ReturnCode -> ExceptT CommandError (Eff BaseScreepsEffects) Unit
doTryCommand commandName command = do
 result <- (liftEff command)
 when (result == ok) $ (pure unit)
 when (result /= ok) $ case result of
                            err_not_enough_energy -> throwError OutOfEnergyError
                            err_not_enough_resources -> throwError OutOfResourcesError
                            err_not_in_range -> throwError OutOfRangeError
                            _ -> throwError UndistinguishedErrors
