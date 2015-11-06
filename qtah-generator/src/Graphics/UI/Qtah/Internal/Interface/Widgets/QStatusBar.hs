-- This file is part of Qtah.
--
-- Copyright 2015 Bryan Gardiner <bog@khumba.net>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License version 3
-- as published by the Free Software Foundation.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Graphics.UI.Qtah.Internal.Interface.Widgets.QStatusBar (
  aModule,
  c_QStatusBar,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  Type (TInt, TObj, TPtr, TVoid),
  addReqIncludes,
  ident,
  includeStd,
  makeClass,
  mkBoolIsProp,
  mkConstMethod,
  mkCtor,
  mkMethod,
  mkMethod',
  mkProps,
  )
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Listener (c_ListenerQString)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QStatusBar"] $
  QtExport (ExportClass c_QStatusBar) :
  map QtExportSignal signals

c_QStatusBar =
  addReqIncludes [includeStd "QStatusBar"] $
  makeClass (ident "QStatusBar") Nothing [c_QWidget]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  ] $
  [ mkMethod' "addPermanentWidget" "addPermanentWidget" [TPtr $ TObj c_QWidget] TVoid
  , mkMethod' "addPermanentWidget" "addPermanentWidgetWithStretch"
    [TPtr $ TObj c_QWidget, TInt] TVoid
  , mkMethod' "addWidget" "addWidget" [TPtr $ TObj c_QWidget] TVoid
  , mkMethod' "addWidget" "addWidgetWithStretch" [TPtr $ TObj c_QWidget, TInt] TVoid
  , mkMethod "clearMessage" [] TVoid
  , mkConstMethod "currentMessage" [] $ TObj c_QString
  , mkMethod' "insertPermanentWidget" "insertPermanentWidget" [TInt, TPtr $ TObj c_QWidget] TVoid
  , mkMethod' "insertPermanentWidget" "insertPermanentWidgetWithStretch"
    [TInt, TPtr $ TObj c_QWidget, TInt] TVoid
  , mkMethod' "insertWidget" "insertWidget" [TInt, TPtr $ TObj c_QWidget] TVoid
  , mkMethod' "insertWidget" "insertWidgetWithStretch" [TInt, TPtr $ TObj c_QWidget, TInt] TVoid
  , mkMethod "removeWidget" [TPtr $ TObj c_QWidget] TVoid
  , mkMethod' "showMessage" "showMessage" [TObj c_QString] TVoid
  , mkMethod' "showMessage" "showMessageWithTimeout" [TObj c_QString, TInt] TVoid
  ] ++
  mkProps
  [ mkBoolIsProp "sizeGripEnabled"
  ]

signals =
  [ makeSignal c_QStatusBar "messageChanged" c_ListenerQString
  ]
