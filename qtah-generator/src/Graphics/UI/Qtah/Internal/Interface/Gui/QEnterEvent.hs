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

module Graphics.UI.Qtah.Internal.Interface.Gui.QEnterEvent (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Type (TInt, TObj),
  addReqIncludes,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  )
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QEvent (c_QEvent)
import Graphics.UI.Qtah.Internal.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Internal.Interface.Core.QPointF (c_QPointF)

{-# ANN module "HLint: ignore Use camelCase" #-}

minVersion = [5, 0]

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Gui", "QEnterEvent"] minVersion
  [ QtExportEvent c_QEnterEvent ]

c_QEnterEvent =
  addReqIncludes [includeStd "QEnterEvent"] $
  makeClass (ident "QEnterEvent") Nothing [c_QEvent]
  [ mkCtor "new" [TObj c_QPointF, TObj c_QPointF, TObj c_QPointF]
  ]
  [ mkConstMethod "globalPos" [] $ TObj c_QPoint
  , mkConstMethod "globalX" [] TInt
  , mkConstMethod "globalY" [] TInt
  , mkConstMethod "localPos" [] $ TObj c_QPointF
  , mkConstMethod "pos" [] $ TObj c_QPoint
  , mkConstMethod "screenPos" [] $ TObj c_QPointF
  , mkConstMethod "windowPos" [] $ TObj c_QPointF
  , mkConstMethod "x" [] TInt
  , mkConstMethod "y" [] TInt
  ]
