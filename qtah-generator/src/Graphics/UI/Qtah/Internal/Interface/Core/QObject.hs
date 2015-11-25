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

module Graphics.UI.Qtah.Internal.Interface.Core.QObject (
  aModule,
  c_QObject,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  Type (TBool, TInt, TObj, TObjToHeap, TPtr, TVoid),
  addReqIncludes,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkMethod,
  mkMethod',
  mkProp,
  mkProps,
  )
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Internal.Flags (qtVersion)
import Graphics.UI.Qtah.Internal.Generator.Types
import {-# SOURCE #-} Graphics.UI.Qtah.Internal.Interface.Listener
import {-# SOURCE #-} Graphics.UI.Qtah.Internal.Interface.Core.QList (c_QListQObject)
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
  collect
  [ just $ mkMethod "blockSignals" [TBool] TBool
  , just $ mkMethod' "children" "childrenNew" [] $ TObjToHeap c_QListQObject
    -- TODO connect
  , just $ mkMethod "deleteLater" [] TVoid
    -- TODO disconnect
  , just $ mkMethod "dumpObjectInfo" [] TVoid
  , just $ mkMethod "dumpObjectTree" [] TVoid
    -- TODO dynamicPropertyNames (>=4.2)
    -- TODO event
    -- TODO eventFilter
    -- TODO findChild
    -- TODO findChildren
    -- TODO inherits
  , just $ mkMethod "installEventFilter" [TPtr $ TObj c_QObject] TVoid
  , just $ mkConstMethod "isWidgetType" [] TBool
  , -- This is a guess on the version bound.
    test (qtVersion >= [5, 0]) $ mkConstMethod "isWindowType" [] TBool
  , just $ mkMethod "killTimer" [TInt] TVoid
    -- TODO metaObject
    -- TODO moveToThread
    -- TODO property
  , just $ mkMethod "removeEventFilter" [TPtr $ TObj c_QObject] TVoid
    -- TODO setProperty
  , just $ mkConstMethod "signalsBlocked" [] TBool
  , just $ mkMethod "startTimer" [TInt] TInt
    -- TODO thread
  ] ++
  mkProps
  [ mkProp "objectName" $ TObj c_QString
  , mkProp "parent" $ TPtr $ TObj c_QObject
  ]

signals =
  [ makeSignal c_QObject "destroyed" c_ListenerPtrQObject
  , makeSignal c_QObject "objectNameChanged" c_ListenerQString
  ]
