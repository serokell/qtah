{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.Core.QSize (
  cppopModule,
  qtModule,
  c_QSize,
  ) where

import Data.Monoid (mconcat)
import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.Types (e_AspectRatioMode)
import Graphics.UI.Qtah.Internal.Interface.Imports
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsType (HsTyApp, HsTyCon),
  )
#include "../Mk.hs.inc"

cppopModule =
  modifyModule' (makeModule "qsize" "gen_qsize.hpp" "gen_qsize.cpp") $ do
    addModuleHaskellName ["Core", "QSize"]
    addModuleExports exports

qtModule = makeQtModule "Core.QSize" $ map QtExport exports

exports = [ExportClass c_QSize]

this = c_QSize

c_QSize =
  addReqIncludes [includeStd "QSize"] $
  classModifyEncoding
  (\c -> c { classCppCType = Just $ TPtr TInt
           , classCppDecoder = Just $ CppCoderFn (ident "qSizeDecode") $
                               reqInclude $ includeLocal "encode.hpp"
           , classCppEncoder = Just $ CppCoderFn (ident "qSizeEncode") $
                               reqInclude $ includeLocal "encode.hpp"
           , classHaskellType =
             Just HaskellEncoding
             { haskellEncodingType = HsTyCon $ UnQual $ HsIdent "HSize.HSize"
             , haskellEncodingCType = HsTyApp (HsTyCon $ UnQual $ HsIdent "QtahF.Ptr") $
                                      HsTyCon $ UnQual $ HsIdent "QtahFC.CInt"
             , haskellEncodingDecoder = "HSize.decodeInternal"
             , haskellEncodingEncoder = "HSize.encodeInternal"
             , haskellEncodingTypeImports =
               hsQualifiedImport "Graphics.UI.Qtah.Core.HSize" "HSize"
             , haskellEncodingCTypeImports = mconcat [importForForeign, importForForeignC]
             , haskellEncodingFnImports =
               hsQualifiedImport "Graphics.UI.Qtah.Core.HSize" "HSize"
             }
           }) $
  makeClass (ident "QSize") Nothing []
  [ _mkCtor "newNull" []
  , _mkCtor "new" [TInt, TInt]
  ] $
  [ _mkConstMethod "boundedTo" [TObj c_QSize] $ TObj c_QSize
  , _mkConstMethod "expandedTo" [TObj c_QSize] $ TObj c_QSize
  , _mkConstMethod "isEmpty" [] TBool
  , _mkConstMethod "isNull" [] TBool
  , _mkConstMethod "isValid" [] TBool
  , _mkMethod "scale" [TObj c_QSize, TEnum e_AspectRatioMode] TVoid
  , _mkMethod "transpose" [] TVoid
  ] ++
  _props
  [ _mkProp "height" TInt
  , _mkProp "width" TInt
  ]
