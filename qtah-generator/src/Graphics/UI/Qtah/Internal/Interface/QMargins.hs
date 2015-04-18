module Graphics.UI.Qtah.Internal.Interface.QMargins (
  c_QMargins,
  ) where

import Foreign.Cppop.Generator.Spec
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsType (HsTyApp, HsTyCon),
  )

c_QMargins =
  classModifyEncoding
  (\c -> c { classCppCType = Just $ TPtr TInt
           , classCppDecoder = Just $ CppCoderFn $ ident "decodeQMargins"
           , classCppEncoder = Just $ CppCoderFn $ ident "encodeQMargins"
           , classHaskellType =
             Just $ HaskellEncoding
             { haskellEncodingType = HsTyCon $ UnQual $ HsIdent "QMargins.QMargins"
             , haskellEncodingCType = HsTyApp (HsTyCon $ UnQual $ HsIdent "F.Ptr") $
                                      HsTyCon $ UnQual $ HsIdent "FC.CInt"
             , haskellEncodingDecoder = "QMargins.decode"
             , haskellEncodingEncoder = "QMargins.encode"
             }
           }) $
  makeClass (ident "QMargins") Nothing [] [] []
