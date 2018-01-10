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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QProgressBar (
  aModule,
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
  mkConstMethod,
  mkCtor,
  mkMethod,
  mkProp,
  )
import Foreign.Hoppy.Generator.Types (bitspaceT, boolT, enumT, intT, objT, ptrT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (bs_Alignment, e_Orientation)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (c_ListenerInt)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QProgressBar"] $
  QtExport (ExportClass c_QProgressBar) :
  map QtExportSignal signals ++
  collect
  [ test (qtVersion >= [4, 1]) $ QtExport $ ExportEnum e_Direction
  ]

c_QProgressBar =
  addReqIncludes [includeStd "QProgressBar"] $
  classSetEntityPrefix "" $
  makeClass (ident "QProgressBar") Nothing [c_QWidget] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QWidget]

  , just $ mkProp "alignment" $ bitspaceT bs_Alignment
  , test (qtVersion >= [4, 2]) $ mkProp "format" $ objT c_QString
  , test (qtVersion >= [4, 1]) $ mkProp "invertedAppearance" boolT
  , just $ mkProp "maximum" intT
  , just $ mkProp "minimum" intT
  , test (qtVersion >= [4, 1]) $ mkProp "orientation" $ enumT e_Orientation
  , just $ mkMethod "reset" [] voidT
  , test (qtVersion >= [5, 0]) $ mkMethod "resetFormat" [] voidT  -- ADDED-BETWEEN 4.8 5.4
  , just $ mkMethod "setRange" [intT, intT] voidT
  , just $ mkConstMethod "text" [] $ objT c_QString
  , test (qtVersion >= [4, 1]) $ mkProp "textDirection" $ enumT e_Direction
  , just $ mkBoolIsProp "textVisible"
  , just $ mkProp "value" intT
  ]

signals =
  [ makeSignal c_QProgressBar "valueChanged" c_ListenerInt
  ]

-- Introduced in Qt 4.1.
e_Direction =
  makeQtEnum (ident1 "QProgressBar" "Direction") [includeStd "QProgressBar"]
  [ (0, ["top", "to", "bottom"])
  , (1, ["bottom", "to", "top"])
  ]
