{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.QSize (
  mod_QSize,
  c_QSize,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Qt (e_AspectRatioMode)
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsType (HsTyApp, HsTyCon),
  )

{-# ANN module "HLint: ignore Use camelCase" #-}

this = c_QSize
thisQt = qtc_QSize
#include "MkQt.hs.inc"

mod_QSize =
  makeQtModule "QSize"
  [ "qualified Graphics.UI.Qtah.H.HSize as HSize" ]
  [ QtExportClass qtc_QSize ]

c_QSize = qtClassClass qtc_QSize

qtc_QSize =
  makeQtClass' [] $
  classModifyEncoding
  (\c -> c { classCppCType = Just $ TPtr TInt
           , classCppDecoder = Just $ CppCoderFn $ ident "qSizeDecode"
           , classCppEncoder = Just $ CppCoderFn $ ident "qSizeEncode"
           , classHaskellType =
             Just $ HaskellEncoding
             { haskellEncodingType = HsTyCon $ UnQual $ HsIdent "HSize.HSize"
             , haskellEncodingCType = HsTyApp (HsTyCon $ UnQual $ HsIdent "F.Ptr") $
                                      HsTyCon $ UnQual $ HsIdent "FC.CInt"
             , haskellEncodingDecoder = "HSize.decodeInternal"
             , haskellEncodingEncoder = "HSize.encodeInternal"
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
