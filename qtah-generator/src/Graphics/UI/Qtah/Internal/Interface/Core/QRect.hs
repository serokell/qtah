{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.Core.QRect (
  cppopModule,
  qtModule,
  c_QRect,
  ) where

import Data.Monoid (mconcat)
import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Internal.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Internal.Interface.Imports
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsType (HsTyCon),
  )
#include "../Mk.hs.inc"

cppopModule = makeCppopModule "Core" "QRect" qtModule

qtModule =
  makeQtModule "Core.QRect"
  [ QtExport $ ExportClass c_QRect ]

this = c_QRect

c_QRect =
  addReqIncludes [includeStd "QRect"] $
  classModifyConversions
  (\c -> c { classHaskellConversion =
             Just ClassHaskellConversion
             { classHaskellConversionType = HsTyCon $ UnQual $ HsIdent "HRect.HRect"
             , classHaskellConversionTypeImports =
               hsQualifiedImport "Graphics.UI.Qtah.Core.HRect" "HRect"
             , classHaskellConversionToCppFn =
               "qRect_newWithRaw <$> HRect.x <*> HRect.y <*> HRect.width <*> HRect.height"
             , classHaskellConversionToCppImports =
               mconcat
               [ hsImports "Control.Applicative" ["(<$>)", "(<*>)"]
               , hsQualifiedImport "Graphics.UI.Qtah.Core.HRect" "HRect"
               ]
             , classHaskellConversionFromCppFn =
               "\\q -> do\n\
               \  x <- qRect_x q\n\
               \  y <- qRect_y q\n\
               \  w <- qRect_width q\n\
               \  h <- qRect_height q\n\
               \  QtahP.return (HRect.HRect x y w h)"
             , classHaskellConversionFromCppImports =
               mconcat
               [ hsQualifiedImport "Graphics.UI.Qtah.Core.HRect" "HRect"
               , importForPrelude
               ]
             }
           }) $
  makeClass (ident "QRect") Nothing []
  [ _mkCtor "newNull" []
  , _mkCtor "newWithPoints" [TObj c_QPoint, TObj c_QPoint]
  , _mkCtor "newWithPointAndSize" [TObj c_QPoint, TObj c_QSize]
  , _mkCtor "newWithRaw" [TInt, TInt, TInt, TInt]
  ] $
  [ _mkMethod "adjust" [TInt, TInt, TInt, TInt] TVoid
  , _mkConstMethod "adjusted" [TInt, TInt, TInt, TInt] $ TObj c_QRect
  , _mkConstMethod "center" [] $ TObj c_QPoint
  , _mkConstMethod' "contains" "containsPoint" [TObj c_QPoint, TBool] TBool
  , _mkConstMethod' "contains" "containsRect" [TObj c_QRect, TBool] TBool
  , _mkConstMethod "intersected" [TObj c_QRect] $ TObj c_QRect
  , _mkConstMethod "intersects" [TObj c_QRect] TBool
  , _mkConstMethod "isEmpty" [] TBool
  , _mkConstMethod "isNull" [] TBool
  , _mkConstMethod "isValid" [] TBool
  , _mkMethod "moveBottom" [TInt] TVoid
  , _mkMethod "moveBottomLeft" [TObj c_QPoint] TVoid
  , _mkMethod "moveBottomRight" [TObj c_QPoint] TVoid
  , _mkMethod "moveCenter" [TObj c_QPoint] TVoid
  , _mkMethod "moveLeft" [TInt] TVoid
  , _mkMethod "moveRight" [TInt] TVoid
  , _mkMethod "moveTo" [TObj c_QPoint] TVoid
  , _mkMethod "moveTop" [TInt] TVoid
  , _mkMethod "moveTopLeft" [TObj c_QPoint] TVoid
  , _mkMethod "moveTopRight" [TObj c_QPoint] TVoid
  , _mkConstMethod "normalized" [] $ TObj c_QRect
  , _mkMethod "setCoords" [TInt, TInt, TInt, TInt] TVoid
  , _mkMethod "setRect" [TInt, TInt, TInt, TInt] TVoid
  , _mkMethod "translate" [TObj c_QPoint] TVoid
  , _mkConstMethod "translated" [TObj c_QPoint] $ TObj c_QRect
  , _mkMethod "united" [TObj c_QRect] $ TObj c_QRect
  ] ++
  _props
  [ _mkProp "bottom" TInt
  , _mkProp "bottomLeft" $ TObj c_QPoint
  , _mkProp "bottomRight" $ TObj c_QPoint
  , _mkProp "height" TInt
  , _mkProp "left" TInt
  , _mkProp "right" TInt
  , _mkProp "size" $ TObj c_QSize
  , _mkProp "top" TInt
  , _mkProp "topLeft" $ TObj c_QPoint
  , _mkProp "topRight" $ TObj c_QPoint
  , _mkProp "width" TInt
  , _mkProp "x" TInt
  , _mkProp "y" TInt
  ]
