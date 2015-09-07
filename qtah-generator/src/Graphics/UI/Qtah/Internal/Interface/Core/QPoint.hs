module Graphics.UI.Qtah.Internal.Interface.Core.QPoint (
  cppopModule,
  qtModule,
  c_QPoint,
  ) where

import Foreign.Cppop.Generator.Language.Haskell.General (
  addImports,
  indent,
  sayLn,
  )
import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Imports
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsType (HsTyCon),
  )

{-# ANN module "HLint: ignore Use camelCase" #-}

cppopModule = makeCppopModule "Core" "QPoint" qtModule

qtModule =
  makeQtModule "Core.QPoint"
  [ QtExport $ ExportClass c_QPoint ]

c_QPoint =
  addReqIncludes [includeStd "QPoint"] $
  classModifyConversions
  (\c -> c { classHaskellConversion =
             Just ClassHaskellConversion
             { classHaskellConversionType = do
               addImports $ hsQualifiedImport "Graphics.UI.Qtah.Core.HPoint" "HPoint"
               return $ HsTyCon $ UnQual $ HsIdent "HPoint.HPoint"
             , classHaskellConversionToCppFn = do
               addImports $ mconcat [hsImports "Control.Applicative" ["(<$>)", "(<*>)"],
                                     hsQualifiedImport "Graphics.UI.Qtah.Core.HPoint" "HPoint"]
               sayLn "qPoint_new <$> HPoint.x <*> HPoint.y"
             , classHaskellConversionFromCppFn = do
               addImports $ mconcat [hsQualifiedImport "Graphics.UI.Qtah.Core.HPoint" "HPoint",
                                     importForPrelude]
               sayLn "\\q -> do"
               indent $ do
                 sayLn "y <- qPoint_x q"
                 sayLn "x <- qPoint_y q"
                 sayLn "QtahP.return (HPoint.HPoint x y)"
             }
           }) $
  makeClass (ident "QPoint") Nothing []
  [ mkCtor "newNull" []
  , mkCtor "new" [TInt, TInt]
  ] $
  [ mkConstMethod "isNull" [] TBool
  , mkConstMethod "manhattanLength" [] TInt
  ] ++
  mkProps
  [ mkProp "x" TInt
  , mkProp "y" TInt
  ]
