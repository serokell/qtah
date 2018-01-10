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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QDialog (
  aModule,
  c_QDialog,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportEnum, ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkBoolIsProp,
  mkCtor,
  mkMethod,
  mkProp,
  )
import Foreign.Hoppy.Generator.Types (bitspaceT, intT, objT, ptrT, voidT)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (bs_WindowFlags)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (c_Listener, c_ListenerInt)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QDialog"] $
  QtExport (ExportClass c_QDialog) :
  map QtExportSignal signals ++
  [ QtExport $ ExportEnum e_DialogCode ]

c_QDialog =
  addReqIncludes [includeStd "QDialog"] $
  classSetEntityPrefix "" $
  makeClass (ident "QDialog") Nothing [c_QWidget]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , mkCtor "newWithParentAndFlags" [ptrT $ objT c_QWidget, bitspaceT bs_WindowFlags]
  , mkMethod "accept" [] voidT
  , mkMethod "done" [intT] voidT
  , mkMethod "exec" [] intT
  , mkBoolIsProp "modal"
  , mkMethod "open" [] voidT
  , mkMethod "reject" [] voidT
  , mkProp "result" intT
  , mkBoolIsProp "sizeGripEnabled"
  ]

signals =
  [ makeSignal c_QDialog "accepted" c_Listener
  , makeSignal c_QDialog "finished" c_ListenerInt
  , makeSignal c_QDialog "rejected" c_Listener
  ]

e_DialogCode =
  makeQtEnum (ident1 "QDialog" "DialogCode") [includeStd "QDialog"]
  [ (0, ["rejected"])
  , (1, ["accepted"])
  ]
