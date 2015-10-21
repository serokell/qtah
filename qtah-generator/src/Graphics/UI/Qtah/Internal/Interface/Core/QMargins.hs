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

module Graphics.UI.Qtah.Internal.Interface.Core.QMargins (
  hoppyModule,
  qtModule,
  c_QMargins,
  ) where

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

hoppyModule = makeHoppyModule "Core" "QMargins" qtModule

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
