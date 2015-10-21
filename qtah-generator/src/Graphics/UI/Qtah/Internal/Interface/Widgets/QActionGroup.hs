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

module Graphics.UI.Qtah.Internal.Interface.Widgets.QActionGroup (
  hoppyModule,
  qtModule,
  c_QActionGroup,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  Type (TBool, TObj, TPtr, TVoid),
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
import Graphics.UI.Qtah.Internal.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Listener (c_ListenerPtrQAction)
import {-# SOURCE #-} Graphics.UI.Qtah.Internal.Interface.Widgets.QAction (c_QAction)

{-# ANN module "HLint: ignore Use camelCase" #-}

hoppyModule = makeHoppyModule "Widgets" "QActionGroup" qtModule

qtModule =
  makeQtModule "Widgets.QActionGroup" $
  QtExport (ExportClass c_QActionGroup) :
  map QtExportSignal signals

c_QActionGroup =
  addReqIncludes [includeStd "QActionGroup"] $
  makeClass (ident "QActionGroup") Nothing
  [ c_QObject ]
  [ mkCtor "new" [TPtr $ TObj c_QObject]
  ] $
  [ -- TODO actions
    mkMethod' "addAction" "addAction" [TPtr $ TObj c_QAction] $ TPtr $ TObj c_QAction
  , mkMethod' "addAction" "addNewAction" [TObj c_QString] $ TPtr $ TObj c_QAction
    -- TODO addNewActionWithIcon
  , mkConstMethod "checkedAction" [] $ TPtr $ TObj c_QAction
  , mkMethod "removeAction" [TPtr $ TObj c_QAction] TVoid
  , mkMethod "setDisabled" [TBool] TVoid
  ] ++
  mkProps
  [ mkBoolIsProp "enabled"
  , mkBoolIsProp "exclusive"
  , mkBoolIsProp "visible"
  ]

signals =
  [ makeSignal c_QActionGroup "hovered" c_ListenerPtrQAction
  , makeSignal c_QActionGroup "triggered" c_ListenerPtrQAction
  ]
