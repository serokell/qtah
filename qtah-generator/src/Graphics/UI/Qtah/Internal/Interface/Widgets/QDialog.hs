-- This file is part of Qtah.
--
-- Copyright 2015 Bryan Gardiner <bog@khumba.net>
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

module Graphics.UI.Qtah.Internal.Interface.Widgets.QDialog (
  aModule,
  c_QDialog,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportEnum, ExportClass),
  Type (TBitspace, TInt, TObj, TPtr, TVoid),
  addReqIncludes,
  ident,
  ident1,
  includeStd,
  makeClass,
  makeEnum,
  mkBoolIsProp,
  mkCtor,
  mkMethod,
  mkProp,
  mkProps,

  )
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.Types (bs_WindowFlags)
import Graphics.UI.Qtah.Internal.Interface.Listener (c_Listener, c_ListenerInt)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QDialog"] $
  [ QtExport $ ExportClass c_QDialog
  , QtExport $ ExportEnum e_DialogCode
  ] ++ map QtExportSignal signals

c_QDialog =
  addReqIncludes [includeStd "QDialog"] $
  makeClass (ident "QDialog") Nothing [c_QWidget]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  , mkCtor "newWithParentAndFlags" [TPtr $ TObj c_QWidget, TBitspace bs_WindowFlags]
  ] $
  [ mkMethod "accept" [] TVoid
  , mkMethod "done" [TInt] TVoid
  , mkMethod "exec" [] TInt
  , mkMethod "open" [] TVoid
  , mkMethod "reject" [] TVoid
  ] ++
  mkProps
  [ mkBoolIsProp "modal"
  , mkProp "result" TInt
  , mkBoolIsProp "sizeGripEnabled"
  ]

signals =
  [ makeSignal c_QDialog "accepted" c_Listener
  , makeSignal c_QDialog "finished" c_ListenerInt
  , makeSignal c_QDialog "rejected" c_Listener
  ]

e_DialogCode =
  makeEnum (ident1 "QDialog" "DialogCode") Nothing
  [ (0, ["rejected"])
  , (1, ["accepted"])
  ]
