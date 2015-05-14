module Graphics.UI.Qtah.Internal.Interface.Core.QString (
  qtModule,
  c_QString,
  ) where

import qualified Data.Set as S
import Data.Monoid (mempty)
import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsType (HsTyCon),
  )

{-# ANN module "HLint: ignore Use camelCase" #-}

qtModule = makeQtModuleForClass c_QString []

c_QString =
  addReqIncludes [includeStd "QString"] $
  classModifyEncoding
  (\c -> c { classCppCType = Just $ TPtr TChar
           , classCppDecoder = Just $ CppCoderFn (ident "QString") mempty
           , classCppDecodeThenFree = True
           , classCppEncoder =
             Just $ CppCoderExpr [Just "strdup(", Nothing, Just ".toStdString().c_str())"] $
             reqInclude $ includeStd "cstring"
           , classHaskellType = Just HaskellEncoding
                                { haskellEncodingType = HsTyCon $ UnQual $ HsIdent "P.String"
                                , haskellEncodingCType = HsTyCon $ UnQual $ HsIdent "FC.CString"
                                , haskellEncodingDecoder = "FCRS.decodeAndFreeCString"
                                , haskellEncodingEncoder = "FC.newCString"
                                , haskellEncodingImports = S.empty
                                }
           }) $
  makeClass (ident "QString") Nothing [] [] []
