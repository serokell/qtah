module Graphics.UI.Qtah.Internal.Interface.Core.QSize (
  cppopModule,
  qtModule,
  c_QSize,
  ) where

import Data.Monoid (mconcat)
import Foreign.Cppop.Generator.Language.Haskell.General (
  addImports,
  indent,
  sayLn,
  )
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
             { classHaskellConversionType = do
               addImports $ hsQualifiedImport "Graphics.UI.Qtah.Core.HSize" "HSize"
               return $ HsTyCon $ UnQual $ HsIdent "HSize.HSize"
             , classHaskellConversionToCppFn = do
               addImports $ mconcat [hsImports "Control.Applicative" ["(<$>)", "(<*>)"],
                                     hsQualifiedImport "Graphics.UI.Qtah.Core.HSize" "HSize"]
               sayLn "qSize_new <$> HSize.width <*> HSize.height"
             , classHaskellConversionFromCppFn = do
               addImports $ mconcat [hsQualifiedImport "Graphics.UI.Qtah.Core.HSize" "HSize",
                                     importForPrelude]
               sayLn "\\q -> do"
               indent $ do
                 sayLn "w <- qSize_width q"
                 sayLn "h <- qSize_height q"
                 sayLn "QtahP.return (HSize.HSize w h)"
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
