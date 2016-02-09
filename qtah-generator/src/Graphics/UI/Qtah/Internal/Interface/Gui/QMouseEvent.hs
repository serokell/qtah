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

module Graphics.UI.Qtah.Internal.Interface.Gui.QMouseEvent (
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
import Graphics.UI.Qtah.Internal.Interface.Core.QEvent (e_Type)
import Graphics.UI.Qtah.Internal.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Internal.Interface.Core.QPointF (c_QPointF)
import Graphics.UI.Qtah.Internal.Interface.Core.Types (
  bs_KeyboardModifiers,
  bs_MouseButtons,
  bs_MouseEventFlags,
  e_MouseButton,
  e_MouseEventSource,
  )
import Graphics.UI.Qtah.Internal.Interface.Gui.QInputEvent (c_QInputEvent)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QMouseEvent"]
  [ QtExportEvent c_QMouseEvent
  ]

c_QMouseEvent =
  addReqIncludes [includeStd "QMouseEvent"] $
  makeClass (ident "QMouseEvent") Nothing [c_QInputEvent]
  (collect
   [ test (qtVersion < [5, 0]) $ mkCtor "new"
     [TEnum e_Type, TObj c_QPoint, TEnum e_MouseButton, TBitspace bs_MouseButtons,
      TBitspace bs_KeyboardModifiers]
   , test (qtVersion < [5, 0]) $ mkCtor "newWithGlobalPosition"
     [TEnum e_Type, TObj c_QPoint, TObj c_QPoint, TEnum e_MouseButton, TBitspace bs_MouseButtons,
      TBitspace bs_KeyboardModifiers]

   , test (qtVersion >= [5, 0]) $ mkCtor "new"
     [TEnum e_Type, TObj c_QPointF, TEnum e_MouseButton, TBitspace bs_MouseButtons,
      TBitspace bs_KeyboardModifiers]
   , test (qtVersion >= [5, 0]) $ mkCtor "newWithScreenPosition"
     [TEnum e_Type, TObj c_QPointF, TObj c_QPointF, TEnum e_MouseButton, TBitspace bs_MouseButtons,
      TBitspace bs_KeyboardModifiers]
   , test (qtVersion >= [5, 0]) $ mkCtor "newWithWindowAndScreenPosition"
     [TEnum e_Type, TObj c_QPointF, TObj c_QPointF, TObj c_QPointF, TEnum e_MouseButton,
      TBitspace bs_MouseButtons, TBitspace bs_KeyboardModifiers]
   ]) $
  collect
  [ just $ mkConstMethod "button" [] $ TEnum e_MouseButton
  , just $ mkConstMethod "buttons" [] $ TBitspace bs_MouseButtons
  , test (qtVersion >= [5, 3]) $ mkConstMethod "flags" [] $ TBitspace bs_MouseEventFlags
  , just $ mkConstMethod "globalPos" [] $ TObj c_QPoint
  , just $ mkConstMethod "globalX" [] TInt
  , just $ mkConstMethod "globalY" [] TInt
  , test (qtVersion >= [5, 0]) $ mkConstMethod "localPos" [] $ TObj c_QPointF
  , just $ mkConstMethod "pos" [] $ TObj c_QPoint
  , test (qtVersion < [5, 0]) $ mkConstMethod "posF" [] $ TObj c_QPointF
  , test (qtVersion >= [5, 0]) $ mkConstMethod "screenPos" [] $ TObj c_QPointF
  , test (qtVersion >= [5, 3]) $ mkConstMethod "source" [] $ TEnum e_MouseEventSource
  , test (qtVersion >= [5, 0]) $ mkConstMethod "windowPos" [] $ TObj c_QPointF
  , just $ mkConstMethod "x" [] TInt
  , just $ mkConstMethod "y" [] TInt
  ]
