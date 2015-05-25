{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.Core.QPoint (
  cppopModule,
  qtModule,
  c_QPoint,
  ) where

import Data.Monoid (mconcat)
import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Imports
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsType (HsTyApp, HsTyCon),
  )
#include "../Mk.hs.inc"

cppopModule = makeCppopModule "Core" "QPoint" qtModule

qtModule =
  makeQtModule "Core.QPoint"
  [ QtExport $ ExportClass c_QPoint ]

this = c_QPoint

c_QPoint =
  addReqIncludes [includeStd "QPoint"] $
  classModifyEncoding
  (\c -> c { classCppCType = Just $ TPtr TInt
           , classCppDecoder = Just $ CppCoderFn (ident "qPointDecode") $
                               reqInclude $ includeLocal "encode.hpp"
           , classCppEncoder = Just $ CppCoderFn (ident "qPointEncode") $
                               reqInclude $ includeLocal "encode.hpp"
           , classHaskellType =
             Just HaskellEncoding
             { haskellEncodingType = HsTyCon $ UnQual $ HsIdent "HPoint.HPoint"
             , haskellEncodingCType = HsTyApp (HsTyCon $ UnQual $ HsIdent "QtahF.Ptr") $
                                      HsTyCon $ UnQual $ HsIdent "QtahFC.CInt"
             , haskellEncodingDecoder = "HPoint.decodeInternal"
             , haskellEncodingEncoder = "HPoint.encodeInternal"
             , haskellEncodingTypeImports =
               hsQualifiedImport "Graphics.UI.Qtah.Core.HPoint" "HPoint"
             , haskellEncodingCTypeImports = mconcat [importForForeign, importForForeignC]
             , haskellEncodingFnImports =
               hsQualifiedImport "Graphics.UI.Qtah.Core.HPoint" "HPoint"
             }
           }) $
  makeClass (ident "QPoint") Nothing []
  [ _mkCtor "newNull" []
  , _mkCtor "new" [TInt, TInt]
  ] $
  [ _mkConstMethod "isNull" [] TBool
  , _mkConstMethod "manhattanLength" [] TInt
  ] ++
  _props
  [ _mkProp "x" TInt
  , _mkProp "y" TInt
  ]
