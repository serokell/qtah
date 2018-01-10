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

module Graphics.UI.Qtah.Generator.Interface.Gui.QInputEvent (
  aModule,
  c_QInputEvent,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  )
import Foreign.Hoppy.Generator.Types (bitspaceT, ulongT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QEvent (c_QEvent)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (bs_KeyboardModifiers)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QInputEvent"]
  [ QtExportEvent c_QInputEvent
  ]

c_QInputEvent =
  addReqIncludes [includeStd "QInputEvent"] $
  classSetEntityPrefix "" $
  makeClass (ident "QInputEvent") Nothing [c_QEvent] $
  collect
  [ just $ mkConstMethod "modifiers" [] $ bitspaceT bs_KeyboardModifiers
  , test (qtVersion >= [5, 0]) $ mkConstMethod "timestamp" [] ulongT
  ]
