-- This file is part of Qtah.
--
-- Copyright 2015 Bryan Gardiner <bog@khumba.net>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License version 3
-- as published by the Free Software Foundation.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.Core.QPoint (
  aModule,
  c_QPoint,
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
  ClassConversions (classHaskellConversion),
  ClassHaskellConversion (
      ClassHaskellConversion,
      classHaskellConversionFromCppFn,
      classHaskellConversionToCppFn,
      classHaskellConversionType
  ),
  Export (ExportClass),
  Type (TBool, TInt),
  addReqIncludes,
  classModifyConversions,
  hsImports,
  hsQualifiedImport,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  mkProp,
  mkProps,
  )
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Imports
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsType (HsTyCon),
  )

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QPoint"]
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
