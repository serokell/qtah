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

module Graphics.UI.Qtah.Generator.Interface.Internal.EventListener (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
  ident2,
  includeLocal,
  makeClass,
  mkCtor,
  mkMethod,
  )
import Foreign.Hoppy.Generator.Types (callbackT, objT, ptrT, voidT)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Interface.Internal.Callback (cb_PtrQObjectPtrQEventBool, cb_Void)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Internal", "EventListener"]
  [ QtExport $ ExportClass c_EventListener ]

c_EventListener =
  addReqIncludes [includeLocal "event.hpp"] $
  classSetEntityPrefix "" $
  makeClass (ident2 "qtah" "event" "EventListener") Nothing [c_QObject]
  [ mkCtor "new" [ptrT $ objT c_QObject, callbackT cb_PtrQObjectPtrQEventBool, callbackT cb_Void]
  , mkMethod "doNotNotifyOnDelete" [] voidT
  ]
