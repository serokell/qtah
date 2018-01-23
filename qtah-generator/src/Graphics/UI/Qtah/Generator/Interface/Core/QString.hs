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

module Graphics.UI.Qtah.Generator.Interface.Core.QString (
  aModule,
  c_QString,
  ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mconcat)
#endif
import Foreign.Hoppy.Generator.Language.Haskell (
  addImports,
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
  MethodApplicability (MNormal),
  Operator (OpArray),
  Purity (Nonpure),
  addReqIncludes,
  classSetEntityPrefix,
  classSetHaskellConversion,
  ident,
  ident2,
  includeLocal,
  includeStd,
  makeClass,
  makeFnMethod,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable, Comparable, Equatable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Std.String (c_string)
import Foreign.Hoppy.Generator.Types (charT, constT, intT, objT, ptrT, refT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QByteArray (c_QByteArray)
import Graphics.UI.Qtah.Generator.Interface.Core.QChar (c_QChar)
import Graphics.UI.Qtah.Generator.Interface.Imports
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsType (HsTyCon),
  )

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QString"]
  [ QtExport $ ExportClass c_QString
  ]

c_QString =
  addReqIncludes [includeStd "QString",
                  includeLocal "wrap_qstring.hpp"] $
  classAddFeatures [Assignable, Copyable, Comparable, Equatable] $
  classSetHaskellConversion
    ClassHaskellConversion
    { classHaskellConversionType = Just $ do
      addImports importForPrelude
      return $ HsTyCon $ UnQual $ HsIdent "QtahP.String"
    , classHaskellConversionToCppFn = Just $ do
      addImports $ mconcat [importForForeignC, importForPrelude]
      sayLn "QtahP.flip QtahFC.withCString newFromCString"
    , classHaskellConversionFromCppFn = Just $ sayLn "toStdString"
    } $
  classSetEntityPrefix "" $
  makeClass (ident "QString") Nothing [] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkCtor "newFromByteArray" [objT c_QByteArray]
  , just $ mkCtor "newFromCString" [ptrT $ constT charT]
  , just $ mkConstMethod' OpArray "at" [intT] $ objT c_QChar
  , just $ makeFnMethod (ident2 "qtah" "qstring" "set") "set" MNormal Nonpure
    [refT $ objT c_QString, intT, objT c_QChar] voidT
  , test (qtVersion >= [5, 0]) $ mkConstMethod "toHtmlEscaped" [] $ objT c_QString
  , just $ mkConstMethod "toLatin1" [] $ objT c_QByteArray
  , just $ mkConstMethod "toLocal8Bit" [] $ objT c_QByteArray
  , just $ mkConstMethod "toStdString" [] $ objT c_string
  , just $ mkConstMethod "toUtf8" [] $ objT c_QByteArray
    -- TODO Lots more method here.
  ]
