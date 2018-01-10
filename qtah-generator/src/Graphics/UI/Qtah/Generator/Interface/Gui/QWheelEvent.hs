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

module Graphics.UI.Qtah.Generator.Interface.Gui.QWheelEvent (
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
import Graphics.UI.Qtah.Generator.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.QPointF (c_QPointF)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (
  bs_KeyboardModifiers,
  bs_MouseButtons,
  e_Orientation,
  e_ScrollPhase,
  )
import Graphics.UI.Qtah.Generator.Interface.Gui.QInputEvent (c_QInputEvent)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QWheelEvent"]
  [ QtExportEvent c_QWheelEvent
  ]

c_QWheelEvent =
  addReqIncludes [includeStd "QWheelEvent"] $
  classSetEntityPrefix "" $
  makeClass (ident "QWheelEvent") Nothing [c_QInputEvent] $
  collect
  [ test (qtVersion < [5, 0]) $ mkCtor "new"
    [objT c_QPoint, intT, bitspaceT bs_MouseButtons, bitspaceT bs_KeyboardModifiers,
     enumT e_Orientation]
  , test (qtVersion < [5, 0]) $ mkCtor "newWithGlobalPosition"
    [objT c_QPoint, objT c_QPoint, intT, bitspaceT bs_MouseButtons, bitspaceT bs_KeyboardModifiers,
     enumT e_Orientation]

  , test (qtVersion >= [5, 0]) $ mkCtor "new"
    [objT c_QPointF, objT c_QPointF, objT c_QPoint, objT c_QPoint, intT, enumT e_Orientation,
     bitspaceT bs_MouseButtons, bitspaceT bs_KeyboardModifiers]
  , test (qtVersion >= [5, 2]) $ mkCtor "newWithPhase"
    [objT c_QPointF, objT c_QPointF, objT c_QPoint, objT c_QPoint, intT, enumT e_Orientation,
     bitspaceT bs_MouseButtons, bitspaceT bs_KeyboardModifiers, enumT e_ScrollPhase]
  , test (qtVersion >= [5, 0]) $ mkConstMethod "angleDelta" [] $ objT c_QPoint
  , just $ mkConstMethod "buttons" [] $ bitspaceT bs_MouseButtons
  , test (qtVersion < [5, 0]) $ mkConstMethod "delta" [] intT
  , just $ mkConstMethod "globalPos" [] $ objT c_QPoint
  , test (qtVersion >= [5, 0]) $ mkConstMethod "globalPosF" [] $ objT c_QPointF
  , just $ mkConstMethod "globalX" [] intT
  , just $ mkConstMethod "globalY" [] intT
  , test (qtVersion >= [5, 2]) $ mkConstMethod "phase" [] $ enumT e_ScrollPhase
  , test (qtVersion >= [5, 0]) $ mkConstMethod "pixelDelta" [] $ objT c_QPoint
  , just $ mkConstMethod "pos" [] $ objT c_QPoint
  , test (qtVersion >= [5, 0]) $ mkConstMethod "posF" [] $ objT c_QPointF
  , just $ mkConstMethod "x" [] intT
  , just $ mkConstMethod "y" [] intT
  ]
