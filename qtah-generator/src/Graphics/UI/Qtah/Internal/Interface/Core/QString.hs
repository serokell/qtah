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
           , classHaskellType =
             Just HaskellEncoding
             { haskellEncodingType = HsTyCon $ UnQual $ HsIdent "QtahP.String"
             , haskellEncodingCType = HsTyCon $ UnQual $ HsIdent "QtahFC.CString"
             , haskellEncodingDecoder = "QtahFCRS.decodeAndFreeCString"
             , haskellEncodingEncoder = "QtahFC.newCString"
             , haskellEncodingTypeImports = S.singleton "qualified Prelude as QtahP"
             , haskellEncodingCTypeImports = S.singleton "qualified Foreign.C as QtahFC"
             , haskellEncodingFnImports =
               S.fromList
               [ "qualified Foreign.C as QtahFC"
               , "qualified Foreign.Cppop.Runtime.Support as QtahFCRS"
               ]
             }
           }) $
  makeClass (ident "QString") Nothing [] [] []
