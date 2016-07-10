-- This file is part of Qtah.
--
-- Copyright 2015-2016 Bryan Gardiner <bog@khumba.net>
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

module Graphics.UI.Qtah.Generator.Interface.Core.QObject (
  aModule,
  c_QObject,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  addReqIncludes,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  mkMethod,
  mkProp,
  mkProps,
  )
import Foreign.Hoppy.Generator.Types (boolT, intT, objT, ptrT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Listener
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Core.QList (c_QListQObject)
import Graphics.UI.Qtah.Generator.Interface.Core.QEvent (c_QEvent)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QObject"] $
  [ QtExport $ ExportClass c_QObject
  ] ++ map QtExportSignal signals

c_QObject =
  addReqIncludes [includeStd "QObject"] $
  makeClass (ident "QObject") Nothing []
  [ mkCtor "new" []
  , mkCtor "newWithParent" [ptrT $ objT c_QObject]
  ] $
  collect
  [ just $ mkMethod "blockSignals" [boolT] boolT
  , just $ mkMethod "children" [] $ objT c_QListQObject
    -- TODO connect
  , just $ mkMethod "deleteLater" [] voidT
    -- TODO disconnect
  , just $ mkMethod "dumpObjectInfo" [] voidT
  , just $ mkMethod "dumpObjectTree" [] voidT
    -- TODO dynamicPropertyNames (>=4.2)
  , just $ mkMethod "event" [ptrT $ objT c_QEvent] boolT
  , just $ mkMethod "eventFilter" [ptrT $ objT c_QObject, ptrT $ objT c_QEvent] boolT
    -- TODO findChild
    -- TODO findChildren
    -- TODO inherits
  , just $ mkMethod "installEventFilter" [ptrT $ objT c_QObject] voidT
  , just $ mkConstMethod "isWidgetType" [] boolT
  , -- This is a guess on the version bound.
    test (qtVersion >= [5, 0]) $ mkConstMethod "isWindowType" [] boolT
  , just $ mkMethod "killTimer" [intT] voidT
    -- TODO metaObject
    -- TODO moveToThread
    -- TODO property
  , just $ mkMethod "removeEventFilter" [ptrT $ objT c_QObject] voidT
    -- TODO setProperty
  , just $ mkConstMethod "signalsBlocked" [] boolT
  , just $ mkMethod "startTimer" [intT] intT
    -- TODO thread
  ] ++
  mkProps
  [ mkProp "objectName" $ objT c_QString
  , mkProp "parent" $ ptrT $ objT c_QObject
  ]

signals =
  [ makeSignal c_QObject "destroyed" c_ListenerPtrQObject
  , makeSignal c_QObject "objectNameChanged" c_ListenerQString
  ]
