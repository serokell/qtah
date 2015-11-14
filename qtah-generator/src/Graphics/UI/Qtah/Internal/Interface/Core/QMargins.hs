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

module Graphics.UI.Qtah.Internal.Interface.Core.QMargins (
  aModule,
  c_QMargins,
  ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mconcat)
#endif
import Foreign.Hoppy.Generator.Language.Haskell.General (
  addImports,
  indent,
  sayLn,
  saysLn,
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
  makeQtModule ["Core", "QMargins"]
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
