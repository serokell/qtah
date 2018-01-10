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

module Graphics.UI.Qtah.Generator.Interface.Core.QStringListModel (
  aModule,
  c_QStringListModel,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkCtor,
  mkProp,
  )
import Foreign.Hoppy.Generator.Types (objT, ptrT)
import Graphics.UI.Qtah.Generator.Interface.Core.QAbstractListModel (c_QAbstractListModel)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Interface.Core.QStringList (c_QStringList)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QStringListModel"] $
  [ QtExport $ ExportClass c_QStringListModel ]

c_QStringListModel =
  addReqIncludes [includeStd "QStringListModel"] $
  classSetEntityPrefix "" $
  makeClass (ident "QStringListModel") Nothing [c_QAbstractListModel]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [ptrT $ objT c_QObject]
  , mkCtor "newWithContents" [objT c_QStringList]
  , mkCtor "newWithContentsAndParent" [objT c_QStringList, ptrT $ objT c_QObject]
  , mkProp "stringList" $ objT c_QStringList
  ]
