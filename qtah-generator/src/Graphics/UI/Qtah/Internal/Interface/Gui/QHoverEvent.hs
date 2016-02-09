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

module Graphics.UI.Qtah.Internal.Interface.Gui.QHoverEvent (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Type (TBitspace, TEnum, TObj),
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
import Graphics.UI.Qtah.Internal.Interface.Core.QEvent (c_QEvent, e_Type)
import Graphics.UI.Qtah.Internal.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Internal.Interface.Core.QPointF (c_QPointF)
import Graphics.UI.Qtah.Internal.Interface.Core.Types (bs_KeyboardModifiers)
import Graphics.UI.Qtah.Internal.Interface.Gui.QInputEvent (c_QInputEvent)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QHoverEvent"]
  [ QtExportEvent c_QHoverEvent
  ]

c_QHoverEvent =
  addReqIncludes [includeStd "QHoverEvent"] $
  makeClass (ident "QHoverEvent") Nothing
  [if qtVersion >= [5, 0] then c_QInputEvent else c_QEvent]
  (collect
   [ test (qtVersion < [5, 0]) $ mkCtor "new" [TEnum e_Type, TObj c_QPoint, TObj c_QPoint]
   , test (qtVersion >= [5, 0]) $ mkCtor "new" [TEnum e_Type, TObj c_QPointF, TObj c_QPointF]
   , test (qtVersion >= [5, 0]) $ mkCtor "newWithModifiers"
     [TEnum e_Type, TObj c_QPointF, TObj c_QPointF, TBitspace bs_KeyboardModifiers]
   ]) $
  collect
  [ just $ mkConstMethod "oldPos" [] $ TObj c_QPoint
  , test (qtVersion >= [5, 0]) $ mkConstMethod "oldPosF" [] $ TObj c_QPointF
  , just $ mkConstMethod "pos" [] $ TObj c_QPoint
  , test (qtVersion >= [5, 0]) $ mkConstMethod "posF" [] $ TObj c_QPointF
  ]
