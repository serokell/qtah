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

module Graphics.UI.Qtah.Generator.Interface.Core.QByteArray (
  aModule,
  c_QByteArray,
  ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mconcat)
#endif
import Foreign.Hoppy.Generator.Language.Haskell (
  addImports,
  indent,
  ln,
  sayLn,
  )
import Foreign.Hoppy.Generator.Spec (
  Class,
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
  hsImport1,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  mkMethod,
  mkMethod',
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable, Comparable, Equatable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (boolT, charT, constT, intT, ptrT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just)
import Graphics.UI.Qtah.Generator.Interface.Imports
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsType (HsTyCon),
  )

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule :: AModule
aModule =
  AQtModule $
  makeQtModule ["Core", "QByteArray"]
  [ QtExport $ ExportClass c_QByteArray
  ]

c_QByteArray :: Class
c_QByteArray =
  addReqIncludes [includeStd "QByteArray"] $
  addAddendum $
  classAddFeatures [Assignable, Copyable, Comparable, Equatable] $
  classSetHaskellConversion conversion $
  classSetEntityPrefix "" $
  makeClass (ident "QByteArray") Nothing [] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkCtor "newFromData" [ptrT $ constT charT]
  , just $ mkCtor "newFromDataAndSize" [ptrT $ constT charT, intT]
  , just $ mkCtor "newFromRepeatedChar" [intT, charT]
  , just $ mkConstMethod "at" [intT] charT
  , just $ mkMethod "clear" [] voidT
  , just $ mkMethod' "data" "getData" [] $ ptrT charT
  , just $ mkConstMethod' "data" "getDataConst" [] $ ptrT $ constT charT
  , just $ mkConstMethod "isEmpty" [] boolT
  , just $ mkConstMethod "isNull" [] boolT
  , just $ mkConstMethod "size" [] intT
    -- TODO Lots more methods.
  ]

conversion :: ClassHaskellConversion
conversion =
  ClassHaskellConversion
  { classHaskellConversionType = Just $ do
    addImports importForByteString
    return $ HsTyCon $ UnQual $ HsIdent "QtahDBS.ByteString"
  , classHaskellConversionToCppFn = Just $ sayLn "convertToCpp"
  , classHaskellConversionFromCppFn = Just $ sayLn "convertFromCpp"
  }

addAddendum :: Class -> Class
addAddendum = addAddendumHaskell $ do
  addImports $ mconcat [hsImport1 "Prelude" "($)",
                        importForByteString,
                        importForByteStringUnsafe,
                        importForPrelude]
  ln
  sayLn "convertToCpp :: QtahDBS.ByteString -> QtahP.IO QByteArray"
  sayLn "convertToCpp ="
  indent $
    sayLn "QtahP.flip QtahDBSU.unsafeUseAsCStringLen $ QtahP.uncurry newFromDataAndSize"
  ln
  sayLn "convertFromCpp :: QByteArrayValue ba => ba -> QtahP.IO QtahDBS.ByteString"
  sayLn "convertFromCpp ba = do"
  indent $ do
    sayLn "d <- getDataConst ba"
    sayLn "len <- size ba"
    sayLn "QtahDBS.packCStringLen (d, len)"
