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

module Graphics.UI.Qtah.Generator.Interface.Internal (modules) where

import qualified Graphics.UI.Qtah.Generator.Interface.Internal.Callback as Callback
import qualified Graphics.UI.Qtah.Generator.Interface.Internal.EventListener as EventListener
import qualified Graphics.UI.Qtah.Generator.Interface.Internal.Listener as Listener
import qualified Graphics.UI.Qtah.Generator.Interface.Internal.SceneEventListener as SceneEventListener
import Graphics.UI.Qtah.Generator.Module (AModule)

{-# ANN module "HLint: ignore Use camelCase" #-}

modules :: [AModule]
modules =
  [ Callback.aModule
  , EventListener.aModule
  , Listener.aModule
  , SceneEventListener.aModule
  ]
