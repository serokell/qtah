{-# LANGUAGE CPP #-}

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
#include "../Mk.hs.inc"

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
  [ _mkCtor "newNull" []
  , _mkCtor "new" [TInt, TInt]
  ] $
  [ _mkConstMethod "boundedTo" [TObj c_QSize] $ TObj c_QSize
  , _mkConstMethod "expandedTo" [TObj c_QSize] $ TObj c_QSize
  , _mkConstMethod "isEmpty" [] TBool
  , _mkConstMethod "isNull" [] TBool
  , _mkConstMethod "isValid" [] TBool
  , _mkMethod "scale" [TObj c_QSize, TEnum e_AspectRatioMode] TVoid
  , _mkMethod "transpose" [] TVoid
  ] ++
  _props
  [ _mkProp "height" TInt
  , _mkProp "width" TInt
  ]
