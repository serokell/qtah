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

module Graphics.UI.Qtah.Internal.Interface.Widgets.QStackedLayout (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass, ExportEnum),
  Type (TEnum, TInt, TObj, TPtr),
  addReqIncludes,
  ident,
  ident1,
  includeStd,
  makeClass,
  makeEnum,
  mkConstMethod,
  mkCtor,
  mkMethod,
  mkProp,
  mkProps,
  )
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Internal.Flags (qtVersion)
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Listener (c_ListenerInt)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QLayout (c_QLayout)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

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
  makeClass (ident "QStackedLayout") Nothing [c_QLayout]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  , mkCtor "newWithLayout" [TPtr $ TObj c_QLayout]
  ] $
  [ mkMethod "addWidget" [TPtr $ TObj c_QWidget] TInt
  , mkConstMethod "count" [] TInt
  , mkMethod "insertWidget" [TInt, TPtr $ TObj c_QWidget] TInt
  , mkConstMethod "widget" [TInt] $ TPtr $ TObj c_QWidget
  ] ++
  (mkProps . collect)
  [ just $ mkProp "currentIndex" TInt
  , just $ mkProp "currentWidget" $ TPtr $ TObj c_QWidget
  , test (qtVersion >= [4, 4]) $ mkProp "stackingMode" $ TEnum e_StackingMode
  ]

signals =
  [ makeSignal c_QStackedLayout "currentChanged" c_ListenerInt
  , makeSignal c_QStackedLayout "widgetRemoved" c_ListenerInt
  ]

e_StackingMode =
  addReqIncludes [includeStd "QStackedLayout"] $
  makeEnum (ident1 "QStackedLayout" "StackingMode") Nothing
  [ (0, ["stack", "one"])
  , (1, ["stack", "all"])
  ]
