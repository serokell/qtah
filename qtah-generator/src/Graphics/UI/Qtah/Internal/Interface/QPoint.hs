{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.QPoint (
  mod_QPoint,
  c_QPoint,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsType (HsTyApp, HsTyCon),
  )

{-# ANN module "HLint: ignore Use camelCase" #-}

this = c_QPoint
thisQt = qtc_QPoint
#include "MkQt.hs.inc"

mod_QPoint =
  makeQtModule "QPoint"
  [ "qualified Graphics.UI.Qtah.H.HPoint as HPoint" ]
  [ QtExportClass qtc_QPoint ]

c_QPoint = qtClassClass qtc_QPoint

qtc_QPoint =
  makeQtClass' [] $
  classModifyEncoding
  (\c -> c { classCppCType = Just $ TPtr TInt
           , classCppDecoder = Just $ CppCoderFn $ ident "qPointDecode"
           , classCppEncoder = Just $ CppCoderFn $ ident "qPointEncode"
           , classHaskellType =
             Just $ HaskellEncoding
             { haskellEncodingType = HsTyCon $ UnQual $ HsIdent "HPoint.HPoint"
             , haskellEncodingCType = HsTyApp (HsTyCon $ UnQual $ HsIdent "F.Ptr") $
                                      HsTyCon $ UnQual $ HsIdent "FC.CInt"
             , haskellEncodingDecoder = "HPoint.decodeInternal"
             , haskellEncodingEncoder = "HPoint.encodeInternal"
             }
           }) $
  makeClass (ident "QPoint") Nothing []
  [ _mkCtor "newNull" []
  , _mkCtor "new" [TInt, TInt]
  ]
  [ _mkConstMethod "isNull" [] TBool
  , _mkConstMethod "manhattanLength" [] TInt
  , _mkMethod "setX" [TInt] TVoid
  , _mkMethod "setY" [TInt] TVoid
  , _mkConstMethod "x" [] TInt
  , _mkConstMethod "y" [] TInt
  ]
