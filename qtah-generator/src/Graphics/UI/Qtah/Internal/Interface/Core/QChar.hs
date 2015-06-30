module Graphics.UI.Qtah.Internal.Interface.Core.QChar (
  cppopModule,
  qtModule,
  c_QChar,
  ) where

import Data.Monoid (mconcat)
import Foreign.Cppop.Generator.Spec
import Foreign.Cppop.Generator.Spec.ClassFeature
import Graphics.UI.Qtah.Internal.Generator.Types
import {-# SOURCE #-} Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Imports
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsType (HsTyCon),
  )

{-# ANN module "HLint: ignore Use camelCase" #-}

cppopModule = makeCppopModule "Core" "QChar" qtModule

qtModule =
  makeQtModule "Core.QChar"
  [ QtExport $ ExportClass c_QChar
  , QtExport $ ExportEnum e_Category
  , QtExport $ ExportEnum e_Decomposition
  , QtExport $ ExportEnum e_Joining
  , QtExport $ ExportEnum e_QCharDirection
  , QtExport $ ExportEnum e_SpecialCharacter
  , QtExport $ ExportEnum e_UnicodeVersion
  ]

this = c_QChar

-- TODO Add more QChar methods.
c_QChar =
  addReqIncludes [includeStd "QChar"] $
  classAddFeatures [Assignable, Copyable, Comparable, Equatable] $
  classModifyConversions
  (\c ->
    let convImports = mconcat [hsImport1 "Prelude" "(.)",
                               importForChar,
                               importForPrelude,
                               importForSupport]
    in c { classHaskellConversion =
           Just ClassHaskellConversion
           { classHaskellConversionType = HsTyCon $ UnQual $ HsIdent "QtahP.Char"
           , classHaskellConversionTypeImports = importForPrelude
           , classHaskellConversionToCppFn =
             "qChar_newFromInt . QtahFCRS.coerceIntegral . QtahDC.ord"
           , classHaskellConversionToCppImports = convImports
           , classHaskellConversionFromCppFn =
             "QtahP.fmap (QtahDC.chr . QtahFCRS.coerceIntegral) . qChar_unicode"
           , classHaskellConversionFromCppImports = convImports
           }
         }) $
  makeClass (ident "QChar") Nothing []
  [ mkCtor this "new" []
  , mkCtor this "newFromCellRow" [TUChar, TUChar]
  , mkCtor this "newFromInt" [TInt]
  , mkCtor this "newFromSpecialCharacter" [TEnum e_SpecialCharacter]
  ]
  [ mkStaticMethod' this "fromAscii" "newFromAscii" [TChar] $ TObj c_QChar
  , mkStaticMethod' this "fromLatin1" "newFromLatin1" [TChar] $ TObj c_QChar
  , mkConstMethod this "category" [] $ TEnum e_Category
  , mkConstMethod this "cell" [] TUChar
  , mkConstMethod this "combiningClass" [] TUChar
  , mkStaticMethod this "currentUnicodeVersion" [] $ TEnum e_UnicodeVersion
  , mkConstMethod this "decomposition" [] $ TObj c_QString
  , mkConstMethod this "decompositionTag" [] $ TEnum e_Decomposition
  , mkConstMethod this "digitValue" [] TInt
  , mkConstMethod this "direction" [] $ TEnum e_QCharDirection
  , mkConstMethod this "hasMirrored" [] TBool
  , mkConstMethod this "isDigit" [] TBool
  , mkConstMethod this "isHighSurrogate" [] TBool
  , mkConstMethod this "isLetter" [] TBool
  , mkConstMethod this "isLetterOrNumber" [] TBool
  , mkConstMethod this "isLowSurrogate" [] TBool
  , mkConstMethod this "isLower" [] TBool
  , mkConstMethod this "isMark" [] TBool
  , mkConstMethod this "isNull" [] TBool
  , mkConstMethod this "isNumber" [] TBool
  , mkConstMethod this "isPrint" [] TBool
  , mkConstMethod this "isPunct" [] TBool
  , mkConstMethod this "isSpace" [] TBool
  , mkConstMethod this "isSymbol" [] TBool
  , mkConstMethod this "isTitleCase" [] TBool
  , mkConstMethod this "isUpper" [] TBool
  , mkConstMethod this "joining" [] $ TEnum e_Joining
  , mkConstMethod this "mirroredChar" [] $ TObj c_QChar
  , mkConstMethod this "row" [] TUChar
  , mkConstMethod this "toAscii" [] TChar
  , mkConstMethod this "toCaseFolded" [] $ TObj c_QChar
  , mkConstMethod this "toLatin1" [] TChar
  , mkConstMethod this "toLower" [] $ TObj c_QChar
  , mkConstMethod this "toTitleCase" [] $ TObj c_QChar
  , mkConstMethod this "toUpper" [] $ TObj c_QChar
  , mkConstMethod' this "unicode" "unicode" [] TUShort
  , mkMethod' this "unicode" "unicodeRef" [] $ TRef TUShort
  , mkConstMethod this "unicodeVersion" [] $ TEnum e_UnicodeVersion
  ]

e_Category =
  makeEnum (ident1 "QChar" "Category") Nothing
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
  makeEnum (ident1 "QChar" "Decomposition") Nothing
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

e_Joining =
  makeEnum (ident1 "QChar" "Joining") Nothing
  [ (3, ["center"])
  , (1, ["dual"])
  , (0, ["other", "joining"])
  , (2, ["right"])
  ]

-- Collides with QBoxLayout::Direction.
e_QCharDirection =
  makeEnum (ident1 "QChar" "Direction") (Just $ toExtName "QCharDirection")
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

e_SpecialCharacter =
  makeEnum (ident1 "QChar" "SpecialCharacter") Nothing
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
  makeEnum (ident1 "QChar" "UnicodeVersion") Nothing
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
