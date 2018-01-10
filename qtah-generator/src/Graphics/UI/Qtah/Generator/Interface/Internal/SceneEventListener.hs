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

module Graphics.UI.Qtah.Generator.Interface.Internal.SceneEventListener (
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
import Graphics.UI.Qtah.Generator.Interface.Internal.Callback
  (cb_PtrQGraphicsItemPtrQEventBool, cb_Void)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QGraphicsItem (c_QGraphicsItem)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Internal", "SceneEventListener"]
  [ QtExport $ ExportClass c_SceneEventListener ]

c_SceneEventListener =
  addReqIncludes [includeLocal "event.hpp"] $
  classSetEntityPrefix "" $
  makeClass (ident2 "qtah" "event" "SceneEventListener") Nothing [c_QGraphicsItem]
  [ mkCtor "new"
      [ptrT $ objT c_QGraphicsItem, callbackT cb_PtrQGraphicsItemPtrQEventBool, callbackT cb_Void]
  , mkMethod "doNotNotifyOnDelete" [] voidT
  ]
