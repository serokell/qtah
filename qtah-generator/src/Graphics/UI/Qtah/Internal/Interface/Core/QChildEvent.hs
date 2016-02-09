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

module Graphics.UI.Qtah.Internal.Interface.Core.QChildEvent (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Type (TBool, TEnum, TObj, TPtr),
  addReqIncludes,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  )
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QEvent (c_QEvent, e_Type)
import Graphics.UI.Qtah.Internal.Interface.Core.QObject (c_QObject)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QChildEvent"]
  [ QtExportEvent c_QChildEvent
  ]

c_QChildEvent =
  addReqIncludes [includeStd "QChildEvent"] $
  makeClass (ident "QChildEvent") Nothing [c_QEvent]
  [ mkCtor "new" [TEnum e_Type, TPtr $ TObj c_QObject]
  ]
  [ mkConstMethod "added" [] TBool
  , mkConstMethod "child" [] $ TPtr $ TObj c_QObject
  , mkConstMethod "polished" [] TBool
  , mkConstMethod "removed" [] TBool
  ]
