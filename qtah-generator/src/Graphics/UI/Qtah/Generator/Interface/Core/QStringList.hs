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

{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Generator.Interface.Core.QStringList (
  aModule,
  c_QStringList,
  ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mconcat)
#endif
import Foreign.Hoppy.Generator.Language.Haskell (
  addImports,
  indent,
  ln,
  sayLn,
  saysLn,
  toHsCastMethodName,
  toHsClassEntityName,
  toHsClassEntityName',
  toHsDataTypeName,
  )
import Foreign.Hoppy.Generator.Spec (
  ClassHaskellConversion (
    ClassHaskellConversion,
    classHaskellConversionFromCppFn,
    classHaskellConversionToCppFn,
    classHaskellConversionType
  ),
  Constness (Const, Nonconst),
  Export (ExportClass),
  addAddendumHaskell,
  addReqIncludes,
  classSetEntityPrefix,
  classSetHaskellConversion,
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
import Foreign.Hoppy.Generator.Types (boolT, enumT, intT, objT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QChar (c_QChar)
import Graphics.UI.Qtah.Generator.Interface.Core.QList (c_QListQString)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (e_CaseSensitivity)
import Graphics.UI.Qtah.Generator.Interface.Imports
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types
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
  classSetHaskellConversion
    ClassHaskellConversion
    { classHaskellConversionType = Just $ do
      addImports importForPrelude
      return $
        HsTyApp (HsTyCon $ Special $ HsListCon) $
        HsTyCon $ UnQual $ HsIdent "QtahP.String"
    , classHaskellConversionToCppFn = Just $ do
      addImports importForRuntime
      sayLn "QtahFHR.fromContents"
    , classHaskellConversionFromCppFn = Just $ do
      addImports importForRuntime
      sayLn "QtahFHR.toContents"
    } $
  addAddendumHaskell addendum $
  classAddFeatures [Assignable, Copyable, Equatable] $
  classSetEntityPrefix "" $
  makeClass (ident "QStringList") Nothing [c_QListQString] $
  collect
  [ just $ mkCtor "new" []
  , -- TODO Regexp methods.
    just $ mkConstMethod' "contains" "containsCase" [objT c_QString, enumT e_CaseSensitivity] boolT
  , just $ mkConstMethod' "filter" "filter" [objT c_QString] $ objT c_QStringList
  , just $ mkConstMethod' "filter" "filterCase" [objT c_QString, enumT e_CaseSensitivity] $
    objT c_QStringList
  , just $ mkConstMethod' "join" "joinString" [objT c_QString] $ objT c_QString
  , test (qtVersion >= [5, 0]) $ mkConstMethod' "join" "joinChar" [objT c_QChar] $ objT c_QString
  , test (qtVersion >= [4, 5]) $ mkMethod "removeDuplicates" [] intT
    -- TODO replaceInStrings.  Ownership?
  , just $ mkMethod' "sort" "sort" [] voidT
  , test (qtVersion >= [5, 0]) $ mkMethod' "sort" "sortCase" [enumT e_CaseSensitivity] voidT
  ]

  where addendum = do
          hsDataTypeName <- toHsDataTypeName Nonconst c_QStringList
          hsDataTypeNameConst <- toHsDataTypeName Const c_QStringList
          castToQList <- toHsCastMethodName Const c_QListQString
          addImports $ mconcat [hsImport1 "Prelude" "(.)",
                                importForPrelude,
                                importForRuntime]
          ln
          saysLn ["instance QtahFHR.HasContents ", hsDataTypeNameConst, " QtahP.String where"]
          indent $
            saysLn ["toContents = QtahFHR.toContents . ", castToQList]
          ln
          saysLn ["instance QtahFHR.HasContents ", hsDataTypeName, " QtahP.String where"]
          indent $
            saysLn ["toContents = QtahFHR.toContents . ", castToQList]
          ln
          saysLn ["instance QtahFHR.FromContents ", hsDataTypeName, " QtahP.String where"]
          indent $ do
            sayLn "fromContents strs' = do"
            indent $ do
              listAppend <- toHsClassEntityName c_QListQString "append"
              saysLn ["l' <- ", toHsClassEntityName' c_QStringList "new"]
              saysLn ["QtahP.mapM_ (", listAppend, " l') strs'"]
              sayLn "QtahP.return l'"
