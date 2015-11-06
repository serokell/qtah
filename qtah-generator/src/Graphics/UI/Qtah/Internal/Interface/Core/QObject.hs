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

module Graphics.UI.Qtah.Internal.Interface.Core.QObject (
  aModule,
  c_QObject,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  Type (TBool, TInt, TObj, TPtr, TVoid),
  addReqIncludes,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkMethod,
  mkProp,
  mkProps,
  )
import Graphics.UI.Qtah.Internal.Generator.Types
import {-# SOURCE #-} Graphics.UI.Qtah.Internal.Interface.Listener
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QObject"] $
  [ QtExport $ ExportClass c_QObject
  ] ++ map QtExportSignal signals

c_QObject =
  addReqIncludes [includeStd "QObject"] $
  makeClass (ident "QObject") Nothing []
  [] $
  [ mkMethod "blockSignals" [TBool] TBool
  , mkMethod "dumpObjectInfo" [] TVoid
  , mkMethod "dumpObjectTree" [] TVoid
  , mkMethod "installEventFilter" [TPtr $ TObj c_QObject] TVoid
  , mkConstMethod "isWidgetType" [] TBool
  , mkMethod "killTimer" [TInt] TVoid
  , mkConstMethod "objectName" [] $ TObj c_QString
  , mkMethod "removeEventFilter" [TPtr $ TObj c_QObject] TVoid
  , mkConstMethod "signalsBlocked" [] TBool
  , mkMethod "startTimer" [TInt] TInt
  ] ++
  mkProps
  [ mkProp "parent" $ TPtr $ TObj c_QObject
  ]

signals =
  [ makeSignal c_QObject "destroyed" c_ListenerPtrQObject
  ]
