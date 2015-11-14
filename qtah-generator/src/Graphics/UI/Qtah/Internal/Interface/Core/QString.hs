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

{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.Core.QString (
  aModule,
  c_QString,
  ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mconcat)
#endif
import Foreign.Hoppy.Generator.Language.Haskell.General (
  addImports,
  sayLn,
  )
import Foreign.Hoppy.Generator.Spec (
  ClassConversion (classHaskellConversion),
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
  Type (TChar, TConst, TInt, TObj, TPtr, TRef, TVoid),
  addReqIncludes,
  classModifyConversion,
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
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QChar (c_QChar)
import Graphics.UI.Qtah.Internal.Interface.Imports
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
  classModifyConversion
  (\c -> c { classHaskellConversion =
             Just ClassHaskellConversion
             { classHaskellConversionType = do
               addImports importForPrelude
               return $ HsTyCon $ UnQual $ HsIdent "QtahP.String"
             , classHaskellConversionToCppFn = do
               addImports $ mconcat [importForForeignC, importForPrelude]
               sayLn "QtahP.flip QtahFC.withCString qString_newFromCString"
             , classHaskellConversionFromCppFn = sayLn "qString_toStdString"
             }
           }) $
  makeClass (ident "QString") Nothing []
  [ mkCtor "newFromCString" [TPtr $ TConst TChar]
  ]
  [ mkConstMethod' OpArray "at" [TInt] $ TObj c_QChar
  , makeFnMethod (ident2 "qtah" "qstring" "set") "set" MNormal Nonpure
    [TRef $ TObj c_QString, TInt, TObj c_QChar] TVoid
  , mkConstMethod "toStdString" [] $ TObj c_string
  ]
