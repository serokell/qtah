{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.Core.QSize (
  qtModule,
  c_QSize,
  ) where

import qualified Data.Set as S
import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Qt (e_AspectRatioMode)
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsType (HsTyApp, HsTyCon),
  )

{-# ANN module "HLint: ignore Use camelCase" #-}

qtModule = makeQtModuleForClass c_QSize []

this = c_QSize
#include "../Mk.hs.inc"

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
               S.singleton "qualified Graphics.UI.Qtah.H.HSize as HSize"
             , haskellEncodingCTypeImports =
               S.fromList
               [ "qualified Foreign as QtahF"
               , "qualified Foreign.C as QtahFC"
               ]
             , haskellEncodingFnImports =
               S.singleton "qualified Graphics.UI.Qtah.H.HSize as HSize"
             }
           }) $
  makeClass (ident "QSize") Nothing []
  [ _mkCtor "newNull" []
  , _mkCtor "new" [TInt, TInt]
  ]
  [ _mkConstMethod "boundedTo" [TObj c_QSize] $ TObj c_QSize
  , _mkConstMethod "expandedTo" [TObj c_QSize] $ TObj c_QSize
  , _mkConstMethod "height" [] TInt
  , _mkConstMethod "isEmpty" [] TBool
  , _mkConstMethod "isNull" [] TBool
  , _mkConstMethod "isValid" [] TBool
  , _mkMethod "scale" [TObj c_QSize, TEnum e_AspectRatioMode] TVoid
  , _mkMethod "setHeight" [TInt] TVoid
  , _mkMethod "setWidth" [TInt] TVoid
  , _mkMethod "transpose" [] TVoid
  , _mkConstMethod "width" [] TInt
  ]
