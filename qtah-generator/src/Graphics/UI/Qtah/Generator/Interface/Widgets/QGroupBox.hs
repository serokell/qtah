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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QGroupBox (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkBoolIsProp,
  mkCtor,
  mkProp,
  )
import Foreign.Hoppy.Generator.Types (bitspaceT, objT, ptrT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (bs_Alignment)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (
  c_ListenerBool,
  )
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QGroupBox"] $
  (QtExport $ ExportClass c_QGroupBox) :
  map QtExportSignal signals

c_QGroupBox =
  addReqIncludes [includeStd "QGroupBox"] $
  classSetEntityPrefix "" $
  makeClass (ident "QGroupBox") Nothing [c_QWidget]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , mkCtor "newWithTitle" [objT c_QString]
  , mkCtor "newWithTitleAndParent" [objT c_QString, ptrT $ objT c_QWidget]
  , mkProp "alignment" $ bitspaceT bs_Alignment
  , mkBoolIsProp "checkable"
  , mkBoolIsProp "checked"
  , mkBoolIsProp "flat"
  , mkProp "title" $ objT c_QString
  ]

signals =
  collect
  [ test (qtVersion >= [4, 2]) $ makeSignal c_QGroupBox "clicked" c_ListenerBool
  , just $ makeSignal c_QGroupBox "toggled" c_ListenerBool
  ]
