-- This file is part of Qtah.
--
-- Copyright 2015 Bryan Gardiner <bog@khumba.net>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Lesser General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.Core.QRect (
  aModule,
  c_QRect,
  ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mconcat)
#endif
import Foreign.Hoppy.Generator.Language.Haskell.General (
  addImports,
  indent,
  sayLn,
  )
import Foreign.Hoppy.Generator.Spec (
  ClassConversion (classHaskellConversion),
  ClassHaskellConversion (
      ClassHaskellConversion,
      classHaskellConversionFromCppFn,
      classHaskellConversionToCppFn,
      classHaskellConversionType
  ),
  Export (ExportClass),
  Type (TBool, TInt, TObj, TVoid),
  addReqIncludes,
  classModifyConversion,
  hsImports,
  hsQualifiedImport,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  mkMethod,
  mkProp,
  mkProps,
  )
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

aModule =
  AQtModule $
  makeQtModule ["Core", "QRect"]
  [ QtExport $ ExportClass c_QRect ]

c_QRect =
  addReqIncludes [includeStd "QRect"] $
  classModifyConversion
  (\c -> c { classHaskellConversion =
             Just ClassHaskellConversion
             { classHaskellConversionType = do
               addImports $ hsQualifiedImport "Graphics.UI.Qtah.Core.HRect" "HRect"
               return $ HsTyCon $ UnQual $ HsIdent "HRect.HRect"
             , classHaskellConversionToCppFn = do
               addImports $ mconcat [hsImports "Control.Applicative" ["(<$>)", "(<*>)"],
                                     hsQualifiedImport "Graphics.UI.Qtah.Core.HRect" "HRect"]
               sayLn "qRect_newWithRaw <$> HRect.x <*> HRect.y <*> HRect.width <*> HRect.height"
             , classHaskellConversionFromCppFn = do
               addImports $ mconcat [hsQualifiedImport "Graphics.UI.Qtah.Core.HRect" "HRect",
                                     importForPrelude]
               sayLn "\\q -> do"
               indent $ do
                 sayLn "x <- qRect_x q"
                 sayLn "y <- qRect_y q"
                 sayLn "w <- qRect_width q"
                 sayLn "h <- qRect_height q"
                 sayLn "QtahP.return (HRect.HRect x y w h)"
             }
           }) $
  makeClass (ident "QRect") Nothing []
  [ mkCtor "newNull" []
  , mkCtor "newWithPoints" [TObj c_QPoint, TObj c_QPoint]
  , mkCtor "newWithPointAndSize" [TObj c_QPoint, TObj c_QSize]
  , mkCtor "newWithRaw" [TInt, TInt, TInt, TInt]
  ] $
  [ mkMethod "adjust" [TInt, TInt, TInt, TInt] TVoid
  , mkConstMethod "adjusted" [TInt, TInt, TInt, TInt] $ TObj c_QRect
  , mkConstMethod "center" [] $ TObj c_QPoint
  , mkConstMethod' "contains" "containsPoint" [TObj c_QPoint, TBool] TBool
  , mkConstMethod' "contains" "containsRect" [TObj c_QRect, TBool] TBool
  , mkConstMethod "intersected" [TObj c_QRect] $ TObj c_QRect
  , mkConstMethod "intersects" [TObj c_QRect] TBool
  , mkConstMethod "isEmpty" [] TBool
  , mkConstMethod "isNull" [] TBool
  , mkConstMethod "isValid" [] TBool
  , mkMethod "moveBottom" [TInt] TVoid
  , mkMethod "moveBottomLeft" [TObj c_QPoint] TVoid
  , mkMethod "moveBottomRight" [TObj c_QPoint] TVoid
  , mkMethod "moveCenter" [TObj c_QPoint] TVoid
  , mkMethod "moveLeft" [TInt] TVoid
  , mkMethod "moveRight" [TInt] TVoid
  , mkMethod "moveTo" [TObj c_QPoint] TVoid
  , mkMethod "moveTop" [TInt] TVoid
  , mkMethod "moveTopLeft" [TObj c_QPoint] TVoid
  , mkMethod "moveTopRight" [TObj c_QPoint] TVoid
  , mkConstMethod "normalized" [] $ TObj c_QRect
  , mkMethod "setCoords" [TInt, TInt, TInt, TInt] TVoid
  , mkMethod "setRect" [TInt, TInt, TInt, TInt] TVoid
  , mkMethod "translate" [TObj c_QPoint] TVoid
  , mkConstMethod "translated" [TObj c_QPoint] $ TObj c_QRect
  , mkMethod "united" [TObj c_QRect] $ TObj c_QRect
  ] ++
  mkProps
  [ mkProp "bottom" TInt
  , mkProp "bottomLeft" $ TObj c_QPoint
  , mkProp "bottomRight" $ TObj c_QPoint
  , mkProp "height" TInt
  , mkProp "left" TInt
  , mkProp "right" TInt
  , mkProp "size" $ TObj c_QSize
  , mkProp "top" TInt
  , mkProp "topLeft" $ TObj c_QPoint
  , mkProp "topRight" $ TObj c_QPoint
  , mkProp "width" TInt
  , mkProp "x" TInt
  , mkProp "y" TInt
  ]
