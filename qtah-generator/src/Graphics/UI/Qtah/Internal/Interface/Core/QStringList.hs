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

module Graphics.UI.Qtah.Internal.Interface.Core.QStringList (
  aModule,
  c_QStringList,
  ) where

import Foreign.Hoppy.Generator.Language.Haskell.General (
  addImports,
  indent,
  ln,
  sayLn,
  saysLn,
  toHsCastMethodName,
  toHsDataTypeName,
  toHsMethodName',
  )
import Foreign.Hoppy.Generator.Spec (
  ClassConversions (classHaskellConversion),
  ClassHaskellConversion (
      ClassHaskellConversion,
      classHaskellConversionFromCppFn,
      classHaskellConversionToCppFn,
      classHaskellConversionType
  ),
  Constness (Const, Nonconst),
  Export (ExportClass),
  Type (TBool, TEnum, TInt, TObj, TVoid),
  addAddendumHaskell,
  addReqIncludes,
  classModifyConversions,
  hsImport1,
  ident,
  includeStd,
  makeClass,
  mkConstMethod',
  mkCtor,
  mkMethod,
  mkMethod',
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable, Equatable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Internal.Flags (qtVersion)
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QChar (c_QChar)
import Graphics.UI.Qtah.Internal.Interface.Core.QList (c_QListQString)
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Core.Types (e_CaseSensitivity)
import Graphics.UI.Qtah.Internal.Interface.Imports
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (Special, UnQual),
  HsSpecialCon (HsListCon),
  HsType (HsTyApp, HsTyCon),
  )

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QStringList"]
  [ QtExport $ ExportClass c_QStringList ]

c_QStringList =
  addReqIncludes [includeStd "QStringList"] $
  classModifyConversions
  (\c -> c { classHaskellConversion =
             Just ClassHaskellConversion
             { classHaskellConversionType = do
               addImports importForPrelude
               return $
                 HsTyApp (HsTyCon $ Special $ HsListCon) $
                 HsTyCon $ UnQual $ HsIdent "QtahP.String"
             , classHaskellConversionToCppFn = do
               addImports importForSupport
               sayLn "QtahFHRS.fromContents"
             , classHaskellConversionFromCppFn = do
               addImports importForSupport
               sayLn "QtahFHRS.toContents"
             }
           }) $
  addAddendumHaskell addendum $
  classAddFeatures [Assignable, Copyable, Equatable] $
  makeClass (ident "QStringList") Nothing [c_QListQString]
  [ mkCtor "new" []
  ] $
  collect
  [ -- TODO Regexp methods.
    just $ mkConstMethod' "contains" "containsCase" [TObj c_QString, TEnum e_CaseSensitivity] TBool
  , just $ mkConstMethod' "filter" "filter" [TObj c_QString] $ TObj c_QStringList
  , just $ mkConstMethod' "filter" "filterCase" [TObj c_QString, TEnum e_CaseSensitivity] $
    TObj c_QStringList
  , just $ mkConstMethod' "join" "joinString" [TObj c_QString] $ TObj c_QString
  , test (qtVersion >= [5, 0]) $ mkConstMethod' "join" "joinChar" [TObj c_QChar] $ TObj c_QString
  , test (qtVersion >= [4, 5]) $ mkMethod "removeDuplicates" [] TInt
    -- TODO replaceInStrings.  Ownership?
  , just $ mkMethod' "sort" "sort" [] TVoid
  , test (qtVersion >= [5, 0]) $ mkMethod' "sort" "sortCase" [TEnum e_CaseSensitivity] TVoid
  ]

  where addendum = do
          let hsDataTypeName = toHsDataTypeName Nonconst c_QStringList
              hsDataTypeNameConst = toHsDataTypeName Const c_QStringList
          addImports $ mconcat [hsImport1 "Prelude" "(.)",
                                importForPrelude,
                                importForSupport]
          ln
          saysLn ["instance QtahFHRS.HasContents ", hsDataTypeNameConst, " QtahP.String where"]
          indent $
            saysLn ["toContents = QtahFHRS.toContents . ", toHsCastMethodName Const c_QListQString]
          ln
          saysLn ["instance QtahFHRS.HasContents ", hsDataTypeName, " QtahP.String where"]
          indent $
            saysLn ["toContents = QtahFHRS.toContents . ", toHsCastMethodName Const c_QStringList]
          ln
          saysLn ["instance QtahFHRS.FromContents ", hsDataTypeName, " QtahP.String where"]
          indent $ do
            sayLn "fromContents strs' = do"
            indent $ do
              saysLn ["l' <- ", toHsMethodName' c_QStringList "new"]
              saysLn ["QtahP.mapM_ (", toHsMethodName' c_QListQString "append", " l') strs'"]
              sayLn "QtahP.return l'"
