-- This file is part of Qtah.
--
-- Copyright 2015-2018 The Qtah Authors.
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Lesser General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE FlexibleContexts #-}

-- | General routines for managing Qt signals.
module Graphics.UI.Qtah.Signal (
  Signal (..),
  connect,
  connect_,
  ) where

import Control.Monad (unless)

-- | A signal that can be connected to an instance of the @object@ (C++) class,
-- and when invoked will call a function of the given @handler@ type.
data Signal object handler = Signal
  { internalConnectSignal :: object -> handler -> IO Bool
  , internalName :: String
  }

instance Show (Signal object handler) where
  show signal = concat ["<Signal ", internalName signal, ">"]

-- | Registers a handler function to listen to a signal an object emits.
-- Returns true if the connection succeeded.
connect :: object -> Signal object handler -> handler -> IO Bool
connect = flip internalConnectSignal

-- | Registers a handler function to listen to a signal an object emits, via
-- 'connect'.  If the connection fails, then the program aborts.
connect_ :: Show (Signal object handler) => object -> Signal object handler -> handler -> IO ()
connect_ object signal handler = do
  success <- connect object signal handler
  unless success $ fail $ "connect_: Failed to connect signal " ++ show signal ++ "."
