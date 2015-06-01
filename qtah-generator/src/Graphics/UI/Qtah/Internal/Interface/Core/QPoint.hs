{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.Core.QPoint (
  cppopModule,
  qtModule,
  c_QPoint,
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

cppopModule = makeCppopModule "Core" "QPoint" qtModule

qtModule =
  makeQtModule "Core.QPoint"
  [ QtExport $ ExportClass c_QPoint ]

this = c_QPoint

c_QPoint =
  addReqIncludes [includeStd "QPoint"] $
  classModifyConversions
  (\c -> c { classHaskellConversion =
             Just ClassHaskellConversion
             { classHaskellConversionType = HsTyCon $ UnQual $ HsIdent "HPoint.HPoint"
             , classHaskellConversionTypeImports =
               hsQualifiedImport "Graphics.UI.Qtah.Core.HPoint" "HPoint"
             , classHaskellConversionToCppFn = "qPoint_new <$> HPoint.x <*> HPoint.y"
             , classHaskellConversionToCppImports =
               mconcat
               [ hsImports "Control.Applicative" ["(<$>)", "(<*>)"]
               , hsQualifiedImport "Graphics.UI.Qtah.Core.HPoint" "HPoint"
               ]
             , classHaskellConversionFromCppFn =
               "\\q -> do\n\
               \  y <- qPoint_x q\n\
               \  x <- qPoint_y q\n\
               \  QtahP.return (HPoint.HPoint x y)"
             , classHaskellConversionFromCppImports =
               mconcat
               [ hsQualifiedImport "Graphics.UI.Qtah.Core.HPoint" "HPoint"
               , importForPrelude
               ]
             }
           }) $
  makeClass (ident "QPoint") Nothing []
  [ _mkCtor "newNull" []
  , _mkCtor "new" [TInt, TInt]
  ] $
  [ _mkConstMethod "isNull" [] TBool
  , _mkConstMethod "manhattanLength" [] TInt
  ] ++
  _props
  [ _mkProp "x" TInt
  , _mkProp "y" TInt
  ]
