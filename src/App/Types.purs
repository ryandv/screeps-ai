module App.Types where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)

import Screeps (CMD, MEMORY, TICK, TIME)

type EffScreepsCommand e = Eff (cmd :: CMD, console :: CONSOLE, tick :: TICK, time :: TIME, memory :: MEMORY | e)
type BaseScreepsEffects = (cmd :: CMD, console :: CONSOLE, tick :: TICK, time :: TIME, memory :: MEMORY)
