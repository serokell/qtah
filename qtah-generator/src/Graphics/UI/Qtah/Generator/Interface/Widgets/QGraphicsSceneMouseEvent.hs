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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QGraphicsSceneMouseEvent (
  aModule,
  c_QGraphicsSceneMouseEvent,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  )
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Foreign.Hoppy.Generator.Types (bitspaceT, enumT, objT)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.QPointF (c_QPointF)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (
  bs_KeyboardModifiers,
  e_MouseButton,
  bs_MouseButtons,
  bs_MouseEventFlags,
  bs_KeyboardModifiers,
  e_MouseEventSource,
  e_MouseEventFlag_minVersion,
  e_MouseEventSource_minVersion,
  )
import Graphics.UI.Qtah.Generator.Interface.Widgets.QGraphicsSceneEvent (c_QGraphicsSceneEvent)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QGraphicsSceneMouseEvent"]
  [ QtExportEvent c_QGraphicsSceneMouseEvent
  ]

c_QGraphicsSceneMouseEvent =
  addReqIncludes [includeStd "QGraphicsSceneMouseEvent"] $
  classSetEntityPrefix "" $
  makeClass (ident "QGraphicsSceneMouseEvent") Nothing [c_QGraphicsSceneEvent] $
  collect
  [ just $ mkConstMethod "button" [] $ enumT e_MouseButton
  , just $ mkConstMethod "buttonDownPos" [enumT e_MouseButton] $ objT c_QPointF
  , just $ mkConstMethod "buttonDownScenePos" [enumT e_MouseButton] $ objT c_QPointF
  , just $ mkConstMethod "buttonDownScreenPos" [enumT e_MouseButton] $ objT c_QPoint
  , just $ mkConstMethod "buttons" [] $ bitspaceT bs_MouseButtons
  , test (qtVersion >= e_MouseEventFlag_minVersion) $ mkConstMethod "flags" [] $
      bitspaceT bs_MouseEventFlags
  , just $ mkConstMethod "lastPos" [] $ objT c_QPointF
  , just $ mkConstMethod "lastScenePos" [] $ objT c_QPointF
  , just $ mkConstMethod "lastScreenPos" [] $ objT c_QPoint
  , just $ mkConstMethod "modifiers" [] $ bitspaceT bs_KeyboardModifiers
  , just $ mkConstMethod "pos" [] $ objT c_QPointF
  , just $ mkConstMethod "scenePos" [] $ objT c_QPointF
  , just $ mkConstMethod "screenPos" [] $ objT c_QPoint
  , test (qtVersion >= e_MouseEventSource_minVersion) $ mkConstMethod "source" [] $
      enumT e_MouseEventSource
  ]
