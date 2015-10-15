module Graphics.UI.Qtah.Internal.Interface.Core.QSize (
  hoppyModule,
  qtModule,
  c_QSize,
  ) where

import Foreign.Hoppy.Generator.Language.Haskell.General (
  addImports,
  indent,
  sayLn,
  )
import Foreign.Hoppy.Generator.Spec (
  ClassConversions (classHaskellConversion),
  ClassHaskellConversion (
      ClassHaskellConversion,
      classHaskellConversionFromCppFn,
      classHaskellConversionToCppFn,
      classHaskellConversionType
  ),
  Export (ExportClass),
  Type (TBool, TEnum, TInt, TObj, TVoid),
  addReqIncludes,
  classModifyConversions,
  hsImports,
  hsQualifiedImport,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  mkMethod,
  mkProp,
  mkProps,
  )
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.Types (e_AspectRatioMode)
import Graphics.UI.Qtah.Internal.Interface.Imports
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsType (HsTyCon),
  )

{-# ANN module "HLint: ignore Use camelCase" #-}

hoppyModule = makeHoppyModule "Core" "QSize" qtModule

qtModule =
  makeQtModule "Core.QSize"
  [ QtExport $ ExportClass c_QSize ]

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
  [ mkCtor "newNull" []
  , mkCtor "new" [TInt, TInt]
  ] $
  [ mkConstMethod "boundedTo" [TObj c_QSize] $ TObj c_QSize
  , mkConstMethod "expandedTo" [TObj c_QSize] $ TObj c_QSize
  , mkConstMethod "isEmpty" [] TBool
  , mkConstMethod "isNull" [] TBool
  , mkConstMethod "isValid" [] TBool
  , mkMethod "scale" [TObj c_QSize, TEnum e_AspectRatioMode] TVoid
  , mkMethod "transpose" [] TVoid
  ] ++
  mkProps
  [ mkProp "height" TInt
  , mkProp "width" TInt
  ]
