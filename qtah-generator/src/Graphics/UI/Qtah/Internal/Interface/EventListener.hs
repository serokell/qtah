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

module Graphics.UI.Qtah.Internal.Interface.EventListener (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  addReqIncludes,
  ident2,
  includeLocal,
  makeClass,
  mkCtor,
  )
import Foreign.Hoppy.Generator.Types (callbackT, intT, ptrT)
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Callback (cb_PtrQObjectPtrQEventBool)
import Graphics.UI.Qtah.Internal.Interface.Core.QObject (c_QObject)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Internal", "EventListener"]
  [ QtExport $ ExportClass c_EventListener ]

c_EventListener =
  addReqIncludes [includeLocal "event.hpp"] $
  makeClass (ident2 "qtah" "event" "EventListener") Nothing [c_QObject]
  [ mkCtor "new" [callbackT cb_PtrQObjectPtrQEventBool, ptrT intT]
  ]
  []
