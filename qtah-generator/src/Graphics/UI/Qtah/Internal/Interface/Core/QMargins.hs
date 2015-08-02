module Graphics.UI.Qtah.Internal.Interface.Core.QMargins (
  cppopModule,
  qtModule,
  c_QMargins,
  ) where

import Data.Monoid (mconcat)
import Foreign.Cppop.Generator.Language.Haskell.General (
  addImports,
  indent,
  sayLn,
  saysLn,
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

cppopModule = makeCppopModule "Core" "QMargins" qtModule

qtModule =
  makeQtModule "Core.QMargins"
  [ QtExport $ ExportClass c_QMargins ]

c_QMargins =
  addReqIncludes [includeStd "QMargins"] $
  classModifyConversions
  (\c -> c { classHaskellConversion =
             Just ClassHaskellConversion
             { classHaskellConversionType = do
               addImports $ hsQualifiedImport "Graphics.UI.Qtah.Core.HMargins" "HMargins"
               return $ HsTyCon $ UnQual $ HsIdent "HMargins.HMargins"
             , classHaskellConversionToCppFn = do
               addImports $ mconcat [hsImports "Control.Applicative" ["(<$>)", "(<*>)"],
                                     hsQualifiedImport "Graphics.UI.Qtah.Core.HMargins" "HMargins"]
               saysLn ["qMargins_new <$> HMargins.left <*> HMargins.top <*> HMargins.right <*> ",
                       "HMargins.bottom"]
             , classHaskellConversionFromCppFn = do
               addImports $ mconcat [hsQualifiedImport "Graphics.UI.Qtah.Core.HMargins" "HMargins",
                                     importForPrelude]
               sayLn "\\q -> do"
               indent $ do
                 sayLn "l <- qMargins_left q"
                 sayLn "t <- qMargins_top q"
                 sayLn "r <- qMargins_right q"
                 sayLn "b <- qMargins_bottom q"
                 sayLn "QtahP.return (HMargins.HMargins l t r b)"
             }
           }) $
  makeClass (ident "QMargins") Nothing []
  [ mkCtor "newNull" []
  , mkCtor "new" [TInt, TInt, TInt, TInt]
  ] $
  [ mkConstMethod "isNull" [] TBool
  ] ++
  mkProps
  [ mkProp "bottom" TInt
  , mkProp "left" TInt
  , mkProp "right" TInt
  , mkProp "top" TInt
  ]
