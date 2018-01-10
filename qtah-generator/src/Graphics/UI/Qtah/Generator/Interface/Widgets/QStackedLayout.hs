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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QStackedLayout (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass, ExportEnum),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  mkMethod,
  mkProp,
  )
import Foreign.Hoppy.Generator.Types (enumT, intT, objT, ptrT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (c_ListenerInt)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QLayout (c_QLayout)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QStackedLayout"] $
  collect $
  concat
  [ [ just $ QtExport $ ExportClass c_QStackedLayout
    ]
  , map (just . QtExportSignal) signals
  , [ test (qtVersion >= [4, 4]) $ QtExport $ ExportEnum e_StackingMode
    ]
  ]

c_QStackedLayout =
  addReqIncludes [includeStd "QStackedLayout"] $
  classSetEntityPrefix "" $
  makeClass (ident "QStackedLayout") Nothing [c_QLayout] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , just $ mkCtor "newWithLayout" [ptrT $ objT c_QLayout]
  , just $ mkMethod "addWidget" [ptrT $ objT c_QWidget] intT
  , just $ mkConstMethod "count" [] intT
  , just $ mkProp "currentIndex" intT
  , just $ mkProp "currentWidget" $ ptrT $ objT c_QWidget
  , just $ mkMethod "insertWidget" [intT, ptrT $ objT c_QWidget] intT
  , test (qtVersion >= [4, 4]) $ mkProp "stackingMode" $ enumT e_StackingMode
  , just $ mkConstMethod "widget" [intT] $ ptrT $ objT c_QWidget
  ]

signals =
  [ makeSignal c_QStackedLayout "currentChanged" c_ListenerInt
  , makeSignal c_QStackedLayout "widgetRemoved" c_ListenerInt
  ]

e_StackingMode =
  makeQtEnum (ident1 "QStackedLayout" "StackingMode") [includeStd "QStackedLayout"]
  [ (0, ["stack", "one"])
  , (1, ["stack", "all"])
  ]
