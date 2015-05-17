module Graphics.UI.Qtah.Internal.Interface.Core.QSize (
  cppopModule,
  qtModule,
  c_QSize,
  ) where

import Data.Monoid (mconcat)
import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.Types (e_AspectRatioMode)
import Graphics.UI.Qtah.Internal.Interface.Imports
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsType (HsTyCon),
  )

{-# ANN module "HLint: ignore Use camelCase" #-}

cppopModule = makeCppopModule "Core" "QSize" qtModule

qtModule =
  makeQtModule "Core.QSize"
  [ QtExport $ ExportClass c_QSize ]

this = c_QSize

c_QSize =
  addReqIncludes [includeStd "QSize"] $
  classModifyConversions
  (\c -> c { classHaskellConversion =
             Just ClassHaskellConversion
             { classHaskellConversionType = HsTyCon $ UnQual $ HsIdent "HSize.HSize"
             , classHaskellConversionTypeImports =
               hsQualifiedImport "Graphics.UI.Qtah.Core.HSize" "HSize"
             , classHaskellConversionToCppFn = "qSize_new <$> HSize.width <*> HSize.height"
             , classHaskellConversionToCppImports =
               mconcat
               [ hsImports "Control.Applicative" ["(<$>)", "(<*>)"]
               , hsQualifiedImport "Graphics.UI.Qtah.Core.HSize" "HSize"
               ]
             , classHaskellConversionFromCppFn =
               "\\q -> do\n\
               \  w <- qSize_width q\n\
               \  h <- qSize_height q\n\
               \  QtahP.return (HSize.HSize w h)"
             , classHaskellConversionFromCppImports =
               mconcat
               [ hsQualifiedImport "Graphics.UI.Qtah.Core.HSize" "HSize"
               , importForPrelude
               ]
             }
           }) $
  makeClass (ident "QSize") Nothing []
  [ mkCtor this "newNull" []
  , mkCtor this "new" [TInt, TInt]
  ] $
  [ mkConstMethod this "boundedTo" [TObj c_QSize] $ TObj c_QSize
  , mkConstMethod this "expandedTo" [TObj c_QSize] $ TObj c_QSize
  , mkConstMethod this "isEmpty" [] TBool
  , mkConstMethod this "isNull" [] TBool
  , mkConstMethod this "isValid" [] TBool
  , mkMethod this "scale" [TObj c_QSize, TEnum e_AspectRatioMode] TVoid
  , mkMethod this "transpose" [] TVoid
  ] ++
  mkProps
  [ mkProp this "height" TInt
  , mkProp this "width" TInt
  ]
