{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.Core.QMargins (
  cppopModule,
  qtModule,
  c_QMargins,
  ) where

import Data.Monoid (mconcat)
import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Imports
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsType (HsTyCon),
  )
#include "../Mk.hs.inc"

cppopModule = makeCppopModule "Core" "QMargins" qtModule

qtModule =
  makeQtModule "Core.QMargins"
  [ QtExport $ ExportClass c_QMargins ]

this = c_QMargins

c_QMargins =
  addReqIncludes [includeStd "QMargins"] $
  classModifyConversions
  (\c -> c { classHaskellConversion =
             Just ClassHaskellConversion
             { classHaskellConversionType = HsTyCon $ UnQual $ HsIdent "HMargins.HMargins"
             , classHaskellConversionTypeImports =
               hsQualifiedImport "Graphics.UI.Qtah.Core.HMargins" "HMargins"
             , classHaskellConversionToCppFn =
               "qMargins_new <$> HMargins.left <*> HMargins.top <*> HMargins.right \
               \<*> HMargins.bottom"
             , classHaskellConversionToCppImports =
               mconcat
               [ hsImports "Control.Applicative" ["(<$>)", "(<*>)"]
               , hsQualifiedImport "Graphics.UI.Qtah.Core.HMargins" "HMargins"
               ]
             , classHaskellConversionFromCppFn =
               "\\q -> do\n\
               \  l <- qMargins_left q\n\
               \  t <- qMargins_top q\n\
               \  r <- qMargins_right q\n\
               \  b <- qMargins_bottom q\n\
               \  QtahP.return (HMargins.HMargins l t r b)"
             , classHaskellConversionFromCppImports =
               mconcat
               [ hsQualifiedImport "Graphics.UI.Qtah.Core.HMargins" "HMargins"
               , importForPrelude
               ]
             }
           }) $
  makeClass (ident "QMargins") Nothing []
  [ _mkCtor "newNull" []
  , _mkCtor "new" [TInt, TInt, TInt, TInt]
  ] $
  [ _mkConstMethod "isNull" [] TBool
  ] ++
  _props
  [ _mkProp "bottom" TInt
  , _mkProp "left" TInt
  , _mkProp "right" TInt
  , _mkProp "top" TInt
  ]
