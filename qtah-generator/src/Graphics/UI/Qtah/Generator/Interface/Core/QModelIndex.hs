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

module Graphics.UI.Qtah.Generator.Interface.Core.QModelIndex (
  aModule,
  c_QModelIndex,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
  classSetConversionToGc,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable, Equatable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (
  bitspaceT,
  boolT,
  constT,
  enumT,
  intT,
  objT,
  ptrT,
  )
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Core.QAbstractItemModel (
  c_QAbstractItemModel,
  )
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Core.QVariant (c_QVariant)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (bs_ItemFlags, e_ItemDataRole)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QModelIndex"]
  [ QtExport $ ExportClass c_QModelIndex ]

c_QModelIndex =
  addReqIncludes [includeStd "QModelIndex"] $
  classSetConversionToGc $
  classAddFeatures [Assignable, Copyable, Equatable] $
  classSetEntityPrefix "" $
  makeClass (ident "QModelIndex") Nothing [] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkConstMethod "child" [intT, intT] $ objT c_QModelIndex
  , just $ mkConstMethod "column" [] intT
  , just $ mkConstMethod' "data" "getData" [] $ objT c_QVariant
  , just $ mkConstMethod' "data" "getDataWithRole" [enumT e_ItemDataRole] $ objT c_QVariant
  , test (qtVersion >= [4, 2]) $ mkConstMethod "flags" [] $ bitspaceT bs_ItemFlags
    -- TODO internalId
    -- TODO internalPointer
  , just $ mkConstMethod "isValid" [] boolT
  , just $ mkConstMethod "model" [] $ ptrT $ constT $ objT c_QAbstractItemModel
  , just $ mkConstMethod "parent" [] $ objT c_QModelIndex
  , just $ mkConstMethod "row" [] intT
  , just $ mkConstMethod "sibling" [intT, intT] $ objT c_QModelIndex
  ]
