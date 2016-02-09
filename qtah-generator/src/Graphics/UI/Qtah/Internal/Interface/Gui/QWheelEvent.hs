-- This file is part of Qtah.
--
-- Copyright 2016 Bryan Gardiner <bog@khumba.net>
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

module Graphics.UI.Qtah.Internal.Interface.Gui.QWheelEvent (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Type (TBitspace, TEnum, TInt, TObj),
  addReqIncludes,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  )
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Internal.Flags (qtVersion)
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Internal.Interface.Core.QPointF (c_QPointF)
import Graphics.UI.Qtah.Internal.Interface.Core.Types (
  bs_KeyboardModifiers,
  bs_MouseButtons,
  e_Orientation,
  e_ScrollPhase,
  )
import Graphics.UI.Qtah.Internal.Interface.Gui.QInputEvent (c_QInputEvent)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QWheelEvent"]
  [ QtExportEvent c_QWheelEvent
  ]

c_QWheelEvent =
  addReqIncludes [includeStd "QWheelEvent"] $
  makeClass (ident "QWheelEvent") Nothing [c_QInputEvent]
  (collect
   [ test (qtVersion < [5, 0]) $ mkCtor "new"
     [TObj c_QPoint, TInt, TBitspace bs_MouseButtons, TBitspace bs_KeyboardModifiers,
      TEnum e_Orientation]
   , test (qtVersion < [5, 0]) $ mkCtor "newWithGlobalPosition"
     [TObj c_QPoint, TObj c_QPoint, TInt, TBitspace bs_MouseButtons, TBitspace bs_KeyboardModifiers,
      TEnum e_Orientation]

   , test (qtVersion >= [5, 0]) $ mkCtor "new"
     [TObj c_QPointF, TObj c_QPointF, TObj c_QPoint, TObj c_QPoint, TInt, TEnum e_Orientation,
      TBitspace bs_MouseButtons, TBitspace bs_KeyboardModifiers]
   , test (qtVersion >= [5, 2]) $ mkCtor "newWithPhase"
     [TObj c_QPointF, TObj c_QPointF, TObj c_QPoint, TObj c_QPoint, TInt, TEnum e_Orientation,
      TBitspace bs_MouseButtons, TBitspace bs_KeyboardModifiers, TEnum e_ScrollPhase]
   ]) $
  collect
  [ test (qtVersion >= [5, 0]) $ mkConstMethod "angleDelta" [] $ TObj c_QPoint
  , just $ mkConstMethod "buttons" [] $ TBitspace bs_MouseButtons
  , test (qtVersion < [5, 0]) $ mkConstMethod "delta" [] TInt
  , just $ mkConstMethod "globalPos" [] $ TObj c_QPoint
  , test (qtVersion >= [5, 0]) $ mkConstMethod "globalPosF" [] $ TObj c_QPointF
  , just $ mkConstMethod "globalX" [] TInt
  , just $ mkConstMethod "globalY" [] TInt
  , test (qtVersion >= [5, 2]) $ mkConstMethod "phase" [] $ TEnum e_ScrollPhase
  , test (qtVersion >= [5, 0]) $ mkConstMethod "pixelDelta" [] $ TObj c_QPoint
  , just $ mkConstMethod "pos" [] $ TObj c_QPoint
  , test (qtVersion >= [5, 0]) $ mkConstMethod "posF" [] $ TObj c_QPointF
  , just $ mkConstMethod "x" [] TInt
  , just $ mkConstMethod "y" [] TInt
  ]
