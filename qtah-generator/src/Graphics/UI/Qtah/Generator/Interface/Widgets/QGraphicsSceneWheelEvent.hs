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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QGraphicsSceneWheelEvent (
  aModule,
  c_QGraphicsSceneWheelEvent,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  )
import Foreign.Hoppy.Generator.Types (bitspaceT, enumT, intT, objT)
import Graphics.UI.Qtah.Generator.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.QPointF (c_QPointF)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (
  bs_KeyboardModifiers,
  bs_MouseButtons,
  e_Orientation,
  )
import Graphics.UI.Qtah.Generator.Interface.Widgets.QGraphicsSceneEvent (c_QGraphicsSceneEvent)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QGraphicsSceneWheelEvent"]
  [ QtExportEvent c_QGraphicsSceneWheelEvent
  ]

c_QGraphicsSceneWheelEvent =
  addReqIncludes [includeStd "QGraphicsSceneWheelEvent"] $
  classSetEntityPrefix "" $
  makeClass (ident "QGraphicsSceneWheelEvent") Nothing [c_QGraphicsSceneEvent]
  [ mkConstMethod "buttons" [] $ bitspaceT bs_MouseButtons
  , mkConstMethod "delta" [] intT
  , mkConstMethod "modifiers" [] $ bitspaceT bs_KeyboardModifiers
  , mkConstMethod "orientation" [] $ enumT e_Orientation
  , mkConstMethod "pos" [] $ objT c_QPointF
  , mkConstMethod "scenePos" [] $ objT c_QPointF
  , mkConstMethod "screenPos" [] $ objT c_QPoint
  ]
