{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.Core.QMargins (
  cppopModule,
  qtModule,
  c_QMargins,
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

cppopModule = makeCppopModule "Core" "QMargins" qtModule

qtModule =
  makeQtModule "Core.QMargins"
  [ QtExport $ ExportClass c_QMargins ]

this = c_QMargins

c_QMargins =
  addReqIncludes [includeStd "QMargins"] $
  classModifyEncoding
  (\c -> c { classCppCType = Just $ TPtr TInt
           , classCppDecoder = Just $ CppCoderFn (ident "qMarginsDecode") $
                               reqInclude $ includeLocal "encode.hpp"
           , classCppEncoder = Just $ CppCoderFn (ident "qMarginsEncode") $
                               reqInclude $ includeLocal "encode.hpp"
           , classHaskellType =
             Just HaskellEncoding
             { haskellEncodingType = HsTyCon $ UnQual $ HsIdent "HMargins.HMargins"
             , haskellEncodingCType = HsTyApp (HsTyCon $ UnQual $ HsIdent "QtahF.Ptr") $
                                      HsTyCon $ UnQual $ HsIdent "QtahFC.CInt"
             , haskellEncodingDecoder = "HMargins.decodeInternal"
             , haskellEncodingEncoder = "HMargins.encodeInternal"
             , haskellEncodingTypeImports =
               hsQualifiedImport "Graphics.UI.Qtah.Core.HMargins" "HMargins"
             , haskellEncodingCTypeImports = mconcat [importForForeign, importForForeignC]
             , haskellEncodingFnImports =
               hsQualifiedImport "Graphics.UI.Qtah.Core.HMargins" "HMargins"
             }
           }) $
  makeClass (ident "QMargins") Nothing []
  [ _mkCtor "newNull" []
  , _mkCtor "new" [TInt, TInt, TInt, TInt]
  ] $
  [ _mkConstMethod "isNull" [] TBool
  ] ++
  _props
  [ _mkProp "bottom" TInt
  , _mkProp "left" TInt
  , _mkProp "right" TInt
  , _mkProp "top" TInt
  ]
