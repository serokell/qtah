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

mod_QMargins =
  makeQtModule "QMargins"
  [ "qualified Graphics.UI.Qtah.H.HMargins as HMargins" ]
  [ QtExportClass qtc_QMargins ]

c_QMargins = qtClassClass qtc_QMargins

qtc_QMargins =
  makeQtClass' [] $
  classModifyEncoding
  (\c -> c { classCppCType = Just $ TPtr TInt
           , classCppDecoder = Just $ CppCoderFn $ ident "decodeQMargins"
           , classCppEncoder = Just $ CppCoderFn $ ident "encodeQMargins"
           , classHaskellType =
             Just $ HaskellEncoding
             { haskellEncodingType = HsTyCon $ UnQual $ HsIdent "HMargins.HMargins"
             , haskellEncodingCType = HsTyApp (HsTyCon $ UnQual $ HsIdent "F.Ptr") $
                                      HsTyCon $ UnQual $ HsIdent "FC.CInt"
             , haskellEncodingDecoder = "HMargins.decode"
             , haskellEncodingEncoder = "HMargins.encode"
             }
           }) $
  makeClass (ident "QMargins") Nothing [] [] []
