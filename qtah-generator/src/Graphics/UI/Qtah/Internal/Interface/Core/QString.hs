module Graphics.UI.Qtah.Internal.Interface.Core.QString (
  cppopModule,
  qtModule,
  c_QString,
  ) where

import Data.Monoid (mconcat, mempty)
import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Imports
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsType (HsTyCon),
  )

{-# ANN module "HLint: ignore Use camelCase" #-}

cppopModule = makeCppopModule "Core" "QString" qtModule

qtModule =
  makeQtModule "Core.QString"
  [ QtExport $ ExportClass c_QString ]

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
             , haskellEncodingTypeImports = importForPrelude
             , haskellEncodingCTypeImports = importForForeignC
             , haskellEncodingFnImports = mconcat [importForForeignC, importForSupport]
             }
           }) $
  makeClass (ident "QString") Nothing [] [] []
