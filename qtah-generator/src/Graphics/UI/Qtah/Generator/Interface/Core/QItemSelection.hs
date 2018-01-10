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

module Graphics.UI.Qtah.Generator.Interface.Core.QItemSelection (
  aModule,
  c_QItemSelection,
  ) where

import Foreign.Hoppy.Generator.Language.Haskell (
  HsTypeSide (HsHsSide),
  addImports,
  cppTypeToHsTypeAndUse,
  sayLn,
  )
import Foreign.Hoppy.Generator.Spec (
  ClassHaskellConversion (
    ClassHaskellConversion,
    classHaskellConversionFromCppFn,
    classHaskellConversionToCppFn,
    classHaskellConversionType
    ),
  Export (ExportClass),
  addAddendumHaskell,
  addReqIncludes,
  classSetEntityPrefix,
  classSetHaskellConversion,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  mkMethod,
  mkStaticMethod,
  )
import Foreign.Hoppy.Generator.Types (bitspaceT, boolT, objT, ptrT, voidT)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Core.QItemSelectionModel (
  bs_SelectionFlags,
  )
import Graphics.UI.Qtah.Generator.Interface.Core.QItemSelectionRange (c_QItemSelectionRange)
import Graphics.UI.Qtah.Generator.Interface.Core.QList (
  c_QListQItemSelectionRange,
  c_QListQModelIndex,
  inheritHasContents,
  )
import Graphics.UI.Qtah.Generator.Interface.Core.QModelIndex (c_QModelIndex)
import Graphics.UI.Qtah.Generator.Interface.Imports (importForRuntime)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types
import Language.Haskell.Syntax (
  HsQName (Special),
  HsSpecialCon (HsListCon),
  HsType (HsTyApp, HsTyCon),
  )

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QItemSelection"]
  [ QtExport $ ExportClass c_QItemSelection ]

c_QItemSelection =
  addReqIncludes [includeStd "QItemSelection"] $
  -- This conversion mirrors QList but doesn't have a to-C++ conversion.
  classSetHaskellConversion
    ClassHaskellConversion
    { classHaskellConversionType = Just $ do
      hsType <- cppTypeToHsTypeAndUse HsHsSide $ objT c_QItemSelectionRange
      return $ HsTyApp (HsTyCon $ Special $ HsListCon) hsType
    , classHaskellConversionToCppFn = Nothing
    , classHaskellConversionFromCppFn = Just $ do
      addImports importForRuntime
      sayLn "QtahFHR.toContents"
    } $
  addAddendumHaskell
    (inheritHasContents c_QItemSelection c_QListQItemSelectionRange $ objT c_QItemSelectionRange) $
  classSetEntityPrefix "" $
  makeClass (ident "QItemSelection") Nothing [c_QListQItemSelectionRange] $
  [ mkCtor "new" []
  , mkCtor "newWithRange" [objT c_QModelIndex, objT c_QModelIndex]
  , mkConstMethod "contains" [objT c_QModelIndex] boolT
  , mkConstMethod "indexes" [] $ objT c_QListQModelIndex
  , mkMethod "merge" [objT c_QItemSelection, bitspaceT bs_SelectionFlags] voidT
  , mkMethod "select" [objT c_QModelIndex, objT c_QModelIndex] voidT
  , mkStaticMethod "split"
    [objT c_QItemSelectionRange, objT c_QItemSelectionRange, ptrT $ objT c_QItemSelection] voidT
  ]
