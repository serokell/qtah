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

module Graphics.UI.Qtah.Internal.Interface.Core.QChar (
  aModule,
  c_QChar,
  ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mconcat)
#endif
import Foreign.Hoppy.Generator.Language.Haskell (
  addImports,
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
  Export (ExportClass, ExportEnum),
  Type (TBool, TChar, TInt, TEnum, TObj, TRef, TUChar, TUShort),
  addReqIncludes,
  classModifyConversion,
  hsImport1,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  mkMethod',
  mkStaticMethod,
  mkStaticMethod',
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable, Comparable, Equatable),
  classAddFeatures,
  )
import Graphics.UI.Qtah.Internal.Flag (collect, just, test)
import Graphics.UI.Qtah.Internal.Flags (qtVersion)
import Graphics.UI.Qtah.Internal.Generator.Types
import {-# SOURCE #-} Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Imports
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsType (HsTyCon),
  )

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QChar"] $
  collect
  [ just $ QtExport $ ExportClass c_QChar
  , just $ QtExport $ ExportEnum e_Category
  , just $ QtExport $ ExportEnum e_Decomposition
  , test (qtVersion < [5, 3]) $ QtExport $ ExportEnum e_Joining
  , test (qtVersion >= [5, 3]) $ QtExport $ ExportEnum e_JoiningType
  , just $ QtExport $ ExportEnum e_Direction
  , just $ QtExport $ ExportEnum e_SpecialCharacter
  , just $ QtExport $ ExportEnum e_UnicodeVersion
  ]

-- TODO Add more QChar methods.
c_QChar =
  addReqIncludes [includeStd "QChar"] $
  classAddFeatures [Assignable, Copyable, Comparable, Equatable] $
  classModifyConversion
  (\c ->
    c { classHaskellConversion =
        Just ClassHaskellConversion
        { classHaskellConversionType = do
          addImports importForPrelude
          return $ HsTyCon $ UnQual $ HsIdent "QtahP.Char"
        , classHaskellConversionToCppFn = do
          addImports $ mconcat [hsImport1 "Prelude" "(.)", importForChar, importForRuntime]
          sayLn "qChar_newFromInt . QtahFHR.coerceIntegral . QtahDC.ord"
        , classHaskellConversionFromCppFn = do
          addImports $ mconcat [hsImport1 "Prelude" "(.)", importForChar, importForPrelude,
                                importForRuntime]
          sayLn "QtahP.fmap (QtahDC.chr . QtahFHR.coerceIntegral) . qChar_unicode"
        }
      }) $
  makeClass (ident "QChar") Nothing []
  [ mkCtor "new" []
  , mkCtor "newFromCellRow" [TUChar, TUChar]
  , mkCtor "newFromInt" [TInt]
  , mkCtor "newFromSpecialCharacter" [TEnum e_SpecialCharacter]
  ] $
  collect
  [ test (qtVersion < [5]) $ mkStaticMethod' "fromAscii" "newFromAscii" [TChar] $ TObj c_QChar
  , just $ mkStaticMethod' "fromLatin1" "newFromLatin1" [TChar] $ TObj c_QChar
  , just $ mkConstMethod "category" [] $ TEnum e_Category
  , just $ mkConstMethod "cell" [] TUChar
  , just $ mkConstMethod "combiningClass" [] TUChar
  , just $ mkStaticMethod "currentUnicodeVersion" [] $ TEnum e_UnicodeVersion
  , just $ mkConstMethod "decomposition" [] $ TObj c_QString
  , just $ mkConstMethod "decompositionTag" [] $ TEnum e_Decomposition
  , just $ mkConstMethod "digitValue" [] TInt
  , just $ mkConstMethod "direction" [] $ TEnum e_Direction
  , just $ mkConstMethod "hasMirrored" [] TBool
  , just $ mkConstMethod "isDigit" [] TBool
  , just $ mkConstMethod "isHighSurrogate" [] TBool
  , just $ mkConstMethod "isLetter" [] TBool
  , just $ mkConstMethod "isLetterOrNumber" [] TBool
  , just $ mkConstMethod "isLowSurrogate" [] TBool
  , just $ mkConstMethod "isLower" [] TBool
  , just $ mkConstMethod "isMark" [] TBool
  , just $ mkConstMethod "isNull" [] TBool
  , just $ mkConstMethod "isNumber" [] TBool
  , just $ mkConstMethod "isPrint" [] TBool
  , just $ mkConstMethod "isPunct" [] TBool
  , just $ mkConstMethod "isSpace" [] TBool
  , just $ mkConstMethod "isSymbol" [] TBool
  , just $ mkConstMethod "isTitleCase" [] TBool
  , just $ mkConstMethod "isUpper" [] TBool
  , test (qtVersion < [5, 3]) $ mkConstMethod "joining" [] $ TEnum e_Joining
  , test (qtVersion >= [5, 3]) $ mkConstMethod "joiningType" [] $ TEnum e_JoiningType
  , just $ mkConstMethod "mirroredChar" [] $ TObj c_QChar
  , just $ mkConstMethod "row" [] TUChar
    -- TODO script (>=5.1)
  , test (qtVersion < [5]) $ mkConstMethod "toAscii" [] TChar
  , just $ mkConstMethod "toCaseFolded" [] $ TObj c_QChar
  , just $ mkConstMethod "toLatin1" [] TChar
  , just $ mkConstMethod "toLower" [] $ TObj c_QChar
  , just $ mkConstMethod "toTitleCase" [] $ TObj c_QChar
  , just $ mkConstMethod "toUpper" [] $ TObj c_QChar
  , just $ mkConstMethod' "unicode" "unicode" [] TUShort
  , just $ mkMethod' "unicode" "unicodeRef" [] $ TRef TUShort
  , just $ mkConstMethod "unicodeVersion" [] $ TEnum e_UnicodeVersion
  ]

e_Category =
  makeQtEnum (ident1 "QChar" "Category") [includeStd "QChar"]
  [ -- Normative.
    (1, ["mark", "non", "spacing"])
  , (2, ["mark", "spacing", "combining"])
  , (3, ["mark", "enclosing"])
  , (4, ["number", "decimal", "digit"])
  , (5, ["number", "letter"])
  , (6, ["number", "other"])
  , (7, ["separator", "space"])
  , (8, ["separator", "line"])
  , (9, ["separator", "paragraph"])
  , (10, ["other", "control"])
  , (11, ["other", "format"])
  , (12, ["other", "surrogate"])
  , (13, ["other", "private", "use"])
  , (14, ["other", "not", "assigned"])
    -- Informative.
  , (15, ["letter", "uppercase"])
  , (16, ["letter", "lowercase"])
  , (17, ["letter", "titlecase"])
  , (18, ["letter", "modifier"])
  , (19, ["letter", "other"])
  , (20, ["punctuation", "connector"])
  , (21, ["punctuation", "dash"])
  , (22, ["punctuation", "open"])
  , (23, ["punctuation", "close"])
  , (24, ["punctuation", "initial", "quote"])
  , (25, ["punctuation", "final", "quote"])
  , (26, ["punctuation", "other"])
  , (27, ["symbol", "math"])
  , (28, ["symbol", "currency"])
  , (29, ["symbol", "modifier"])
  , (30, ["symbol", "other"])
  , (0, ["no", "category"])
  ]

e_Decomposition =
  makeQtEnum (ident1 "QChar" "Decomposition") [includeStd "QChar"]
  [ (0, ["no", "decomposition"])
  , (1, ["canonical"])
  , (8, ["circle"])
  , (16, ["compat"])
  , (6, ["final"])
  , (2, ["font"])
  , (17, ["fraction"])
  , (4, ["initial"])
  , (7, ["isolated"])
  , (5, ["medial"])
  , (13, ["narrow"])
  , (3, ["no", "break"])
  , (14, ["small"])
  , (15, ["square"])
  , (10, ["sub"])
  , (9, ["super"])
  , (11, ["vertical"])
  , (12, ["wide"])
  ]

e_Direction =
  makeQtEnum (ident1 "QChar" "Direction") [includeStd "QChar"]
  [ (13, ["dir", "al"])
  , (5, ["dir", "an"])
  , (7, ["dir", "b"])
  , (18, ["dir", "bn"])
  , (6, ["dir", "cs"])
  , (2, ["dir", "en"])
  , (3, ["dir", "es"])
  , (4, ["dir", "et"])
  , (0, ["dir", "l"])
  , (11, ["dir", "lre"])
  , (12, ["dir", "lro"])
  , (17, ["dir", "nsm"])
  , (10, ["dir", "on"])
  , (16, ["dir", "pdf"])
  , (1, ["dir", "r"])
  , (14, ["dir", "rle"])
  , (15, ["dir", "rlo"])
  , (8, ["dir", "s"])
  , (9, ["dir", "ws"])
  ]

-- | Removed in Qt 5.3.0.
e_Joining =
  makeQtEnum (ident1 "QChar" "Joining") [includeStd "QChar"]
  [ (3, ["center"])
  , (1, ["dual"])
  , (0, ["other", "joining"])
  , (2, ["right"])
  ]

-- | Since Qt 5.3.0.
e_JoiningType =
  makeQtEnum (ident1 "QChar" "JoiningType") [includeStd "QChar"]
  [ (0, ["joining", "none"])
  , (1, ["joining", "causing"])
  , (2, ["joining", "dual"])
  , (3, ["joining", "right"])
  , (4, ["joining", "left"])
  , (5, ["joining", "transparent"])
  ]

e_SpecialCharacter =
  makeQtEnum (ident1 "QChar" "SpecialCharacter") [includeStd "QChar"]
  [ (0x0000, ["null"])
  , (0x00a0, ["nbsp"])
  , (0x2028, ["line", "separator"])
  , (0x2029, ["paragraph", "separator"])
  , (0xfffc, ["object", "replacement", "character"])
  , (0xfffd, ["replacement", "character"])
  , (0xfeff, ["byte", "order", "mark"])
  , (0xfffe, ["byte", "order", "swapped"])
  ]

e_UnicodeVersion =
  makeQtEnum (ident1 "QChar" "UnicodeVersion") [includeStd "QChar"]
  [ (1, ["unicode", "1_1"])
  , (2, ["unicode", "2_0"])
  , (3, ["unicode", "2_1_2"])
  , (4, ["unicode", "3_0"])
  , (5, ["unicode", "3_1"])
  , (6, ["unicode", "3_2"])
  , (7, ["unicode", "4_0"])
  , (8, ["unicode", "4_1"])
  , (9, ["unicode", "5_0"])
  , (0, ["unicode", "unassigned"])
  ]
