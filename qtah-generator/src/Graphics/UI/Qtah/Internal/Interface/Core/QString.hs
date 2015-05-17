module Graphics.UI.Qtah.Internal.Interface.Core.QString (
  cppopModule,
  qtModule,
  c_QString,
  ) where

import Data.Monoid (mappend, mempty)
import Foreign.Cppop.Generator.Spec
import Foreign.Cppop.Generator.Std (c_std__string)
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

this = c_QString

c_QString =
  addReqIncludes [includeStd "QString"] $
  classModifyConversions
  (\c -> c { classHaskellConversion =
             Just ClassHaskellConversion
             { classHaskellConversionType = HsTyCon $ UnQual $ HsIdent "QtahP.String"
             , classHaskellConversionTypeImports = importForPrelude
             , classHaskellConversionToCppFn =
               "QtahP.flip QtahFC.withCString qString_newFromCString"
             , classHaskellConversionToCppImports = importForPrelude `mappend` importForForeignC
             , classHaskellConversionFromCppFn = "qString_toStdString"
             , classHaskellConversionFromCppImports = mempty
             }
           }) $
  makeClass (ident "QString") Nothing []
  [ mkCtor this "newFromCString" [TPtr $ TConst TChar]
  ]
  [ mkConstMethod this "toStdString" [] $ TObj c_std__string
  ]
