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

module Graphics.UI.Qtah.Generator.Interface.Gui.QMouseEvent (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  )
import Foreign.Hoppy.Generator.Types (bitspaceT, enumT, intT, objT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QEvent (e_Type)
import Graphics.UI.Qtah.Generator.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.QPointF (c_QPointF)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (
  bs_KeyboardModifiers,
  bs_MouseButtons,
  bs_MouseEventFlags,
  e_MouseButton,
  e_MouseEventSource,
  )
import Graphics.UI.Qtah.Generator.Interface.Gui.QInputEvent (c_QInputEvent)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QMouseEvent"]
  [ QtExportEvent c_QMouseEvent
  ]

c_QMouseEvent =
  addReqIncludes [includeStd "QMouseEvent"] $
  classSetEntityPrefix "" $
  makeClass (ident "QMouseEvent") Nothing [c_QInputEvent] $
  collect
  [ test (qtVersion < [5, 0]) $ mkCtor "new"
    [enumT e_Type, objT c_QPoint, enumT e_MouseButton, bitspaceT bs_MouseButtons,
     bitspaceT bs_KeyboardModifiers]
  , test (qtVersion < [5, 0]) $ mkCtor "newWithGlobalPosition"
    [enumT e_Type, objT c_QPoint, objT c_QPoint, enumT e_MouseButton, bitspaceT bs_MouseButtons,
     bitspaceT bs_KeyboardModifiers]

  , test (qtVersion >= [5, 0]) $ mkCtor "new"
    [enumT e_Type, objT c_QPointF, enumT e_MouseButton, bitspaceT bs_MouseButtons,
     bitspaceT bs_KeyboardModifiers]
  , test (qtVersion >= [5, 0]) $ mkCtor "newWithScreenPosition"
    [enumT e_Type, objT c_QPointF, objT c_QPointF, enumT e_MouseButton, bitspaceT bs_MouseButtons,
     bitspaceT bs_KeyboardModifiers]
  , test (qtVersion >= [5, 0]) $ mkCtor "newWithWindowAndScreenPosition"
    [enumT e_Type, objT c_QPointF, objT c_QPointF, objT c_QPointF, enumT e_MouseButton,
     bitspaceT bs_MouseButtons, bitspaceT bs_KeyboardModifiers]
  , just $ mkConstMethod "button" [] $ enumT e_MouseButton
  , just $ mkConstMethod "buttons" [] $ bitspaceT bs_MouseButtons
  , test (qtVersion >= [5, 3]) $ mkConstMethod "flags" [] $ bitspaceT bs_MouseEventFlags
  , just $ mkConstMethod "globalPos" [] $ objT c_QPoint
  , just $ mkConstMethod "globalX" [] intT
  , just $ mkConstMethod "globalY" [] intT
  , test (qtVersion >= [5, 0]) $ mkConstMethod "localPos" [] $ objT c_QPointF
  , just $ mkConstMethod "pos" [] $ objT c_QPoint
  , test (qtVersion < [5, 0]) $ mkConstMethod "posF" [] $ objT c_QPointF
  , test (qtVersion >= [5, 0]) $ mkConstMethod "screenPos" [] $ objT c_QPointF
  , test (qtVersion >= [5, 3]) $ mkConstMethod "source" [] $ enumT e_MouseEventSource
  , test (qtVersion >= [5, 0]) $ mkConstMethod "windowPos" [] $ objT c_QPointF
  , just $ mkConstMethod "x" [] intT
  , just $ mkConstMethod "y" [] intT
  ]
