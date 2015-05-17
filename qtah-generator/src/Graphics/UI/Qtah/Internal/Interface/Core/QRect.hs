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

{-# ANN module "HLint: ignore Use camelCase" #-}

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
  [ mkCtor this "newNull" []
  , mkCtor this "newWithPoints" [TObj c_QPoint, TObj c_QPoint]
  , mkCtor this "newWithPointAndSize" [TObj c_QPoint, TObj c_QSize]
  , mkCtor this "newWithRaw" [TInt, TInt, TInt, TInt]
  ] $
  [ mkMethod this "adjust" [TInt, TInt, TInt, TInt] TVoid
  , mkConstMethod this "adjusted" [TInt, TInt, TInt, TInt] $ TObj c_QRect
  , mkConstMethod this "center" [] $ TObj c_QPoint
  , mkConstMethod' this "contains" "containsPoint" [TObj c_QPoint, TBool] TBool
  , mkConstMethod' this "contains" "containsRect" [TObj c_QRect, TBool] TBool
  , mkConstMethod this "intersected" [TObj c_QRect] $ TObj c_QRect
  , mkConstMethod this "intersects" [TObj c_QRect] TBool
  , mkConstMethod this "isEmpty" [] TBool
  , mkConstMethod this "isNull" [] TBool
  , mkConstMethod this "isValid" [] TBool
  , mkMethod this "moveBottom" [TInt] TVoid
  , mkMethod this "moveBottomLeft" [TObj c_QPoint] TVoid
  , mkMethod this "moveBottomRight" [TObj c_QPoint] TVoid
  , mkMethod this "moveCenter" [TObj c_QPoint] TVoid
  , mkMethod this "moveLeft" [TInt] TVoid
  , mkMethod this "moveRight" [TInt] TVoid
  , mkMethod this "moveTo" [TObj c_QPoint] TVoid
  , mkMethod this "moveTop" [TInt] TVoid
  , mkMethod this "moveTopLeft" [TObj c_QPoint] TVoid
  , mkMethod this "moveTopRight" [TObj c_QPoint] TVoid
  , mkConstMethod this "normalized" [] $ TObj c_QRect
  , mkMethod this "setCoords" [TInt, TInt, TInt, TInt] TVoid
  , mkMethod this "setRect" [TInt, TInt, TInt, TInt] TVoid
  , mkMethod this "translate" [TObj c_QPoint] TVoid
  , mkConstMethod this "translated" [TObj c_QPoint] $ TObj c_QRect
  , mkMethod this "united" [TObj c_QRect] $ TObj c_QRect
  ] ++
  mkProps
  [ mkProp this "bottom" TInt
  , mkProp this "bottomLeft" $ TObj c_QPoint
  , mkProp this "bottomRight" $ TObj c_QPoint
  , mkProp this "height" TInt
  , mkProp this "left" TInt
  , mkProp this "right" TInt
  , mkProp this "size" $ TObj c_QSize
  , mkProp this "top" TInt
  , mkProp this "topLeft" $ TObj c_QPoint
  , mkProp this "topRight" $ TObj c_QPoint
  , mkProp this "width" TInt
  , mkProp this "x" TInt
  , mkProp this "y" TInt
  ]
