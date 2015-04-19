{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.QMargins (
  mod_QMargins,
  c_QMargins,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsType (HsTyApp, HsTyCon),
  )

{-# ANN module "HLint: ignore Use camelCase" #-}

this = c_QMargins
thisQt = qtc_QMargins
#include "MkQt.hs.inc"

mod_QMargins =
  makeQtModule "QMargins"
  [ "qualified Graphics.UI.Qtah.H.HMargins as HMargins" ]
  [ QtExportClass qtc_QMargins ]

c_QMargins = qtClassClass qtc_QMargins

qtc_QMargins =
  makeQtClass' [] $
  classModifyEncoding
  (\c -> c { classCppCType = Just $ TPtr TInt
           , classCppDecoder = Just $ CppCoderFn $ ident "qMarginsDecode"
           , classCppEncoder = Just $ CppCoderFn $ ident "qMarginsEncode"
           , classHaskellType =
             Just $ HaskellEncoding
             { haskellEncodingType = HsTyCon $ UnQual $ HsIdent "HMargins.HMargins"
             , haskellEncodingCType = HsTyApp (HsTyCon $ UnQual $ HsIdent "F.Ptr") $
                                      HsTyCon $ UnQual $ HsIdent "FC.CInt"
             , haskellEncodingDecoder = "HMargins.decodeInternal"
             , haskellEncodingEncoder = "HMargins.encodeInternal"
             }
           }) $
  makeClass (ident "QMargins") Nothing []
  [ _mkCtor "newNull" []
  , _mkCtor "new" [TInt, TInt, TInt, TInt]
  ]
  [ _mkConstMethod "bottom" [] TInt
  , _mkConstMethod "isNull" [] TBool
  , _mkConstMethod "left" [] TInt
  , _mkConstMethod "right" [] TInt
  , _mkMethod "setBottom" [TInt] TVoid
  , _mkMethod "setLeft" [TInt] TVoid
  , _mkMethod "setRight" [TInt] TVoid
  , _mkMethod "setTop" [TInt] TVoid
  , _mkConstMethod "top" [] TInt
  ]
