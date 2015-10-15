module Graphics.UI.Qtah.Internal.Interface.Core.QChar (
  hoppyModule,
  qtModule,
  c_QChar,
  ) where

import Foreign.Hoppy.Generator.Language.Haskell.General (
  addImports,
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
  Export (ExportClass, ExportEnum),
  Type (TBool, TChar, TInt, TEnum, TObj, TRef, TUChar, TUShort),
  addReqIncludes,
  classModifyConversions,
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
import Graphics.UI.Qtah.Internal.Generator.Types
import {-# SOURCE #-} Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Imports
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsType (HsTyCon),
  )

{-# ANN module "HLint: ignore Use camelCase" #-}

hoppyModule = makeHoppyModule "Core" "QChar" qtModule

qtModule =
  makeQtModule "Core.QChar"
  [ QtExport $ ExportClass c_QChar
  , QtExport $ ExportEnum e_Category
  , QtExport $ ExportEnum e_Decomposition
  , QtExport $ ExportEnum e_Joining
  , QtExport $ ExportEnum e_Direction
  , QtExport $ ExportEnum e_SpecialCharacter
  , QtExport $ ExportEnum e_UnicodeVersion
  ]

-- TODO Add more QChar methods.
c_QChar =
  addReqIncludes [includeStd "QChar"] $
  classAddFeatures [Assignable, Copyable, Comparable, Equatable] $
  classModifyConversions
  (\c ->
    c { classHaskellConversion =
        Just ClassHaskellConversion
        { classHaskellConversionType = do
          addImports importForPrelude
          return $ HsTyCon $ UnQual $ HsIdent "QtahP.Char"
        , classHaskellConversionToCppFn = do
          addImports $ mconcat [hsImport1 "Prelude" "(.)", importForChar, importForSupport]
          sayLn "qChar_newFromInt . QtahFHRS.coerceIntegral . QtahDC.ord"
        , classHaskellConversionFromCppFn = do
          addImports $ mconcat [hsImport1 "Prelude" "(.)", importForChar, importForPrelude,
                                importForSupport]
          sayLn "QtahP.fmap (QtahDC.chr . QtahFHRS.coerceIntegral) . qChar_unicode"
        }
      }) $
  makeClass (ident "QChar") Nothing []
  [ mkCtor "new" []
  , mkCtor "newFromCellRow" [TUChar, TUChar]
  , mkCtor "newFromInt" [TInt]
  , mkCtor "newFromSpecialCharacter" [TEnum e_SpecialCharacter]
  ]
  [ mkStaticMethod' "fromAscii" "newFromAscii" [TChar] $ TObj c_QChar
  , mkStaticMethod' "fromLatin1" "newFromLatin1" [TChar] $ TObj c_QChar
  , mkConstMethod "category" [] $ TEnum e_Category
  , mkConstMethod "cell" [] TUChar
  , mkConstMethod "combiningClass" [] TUChar
  , mkStaticMethod "currentUnicodeVersion" [] $ TEnum e_UnicodeVersion
  , mkConstMethod "decomposition" [] $ TObj c_QString
  , mkConstMethod "decompositionTag" [] $ TEnum e_Decomposition
  , mkConstMethod "digitValue" [] TInt
  , mkConstMethod "direction" [] $ TEnum e_Direction
  , mkConstMethod "hasMirrored" [] TBool
  , mkConstMethod "isDigit" [] TBool
  , mkConstMethod "isHighSurrogate" [] TBool
  , mkConstMethod "isLetter" [] TBool
  , mkConstMethod "isLetterOrNumber" [] TBool
  , mkConstMethod "isLowSurrogate" [] TBool
  , mkConstMethod "isLower" [] TBool
  , mkConstMethod "isMark" [] TBool
  , mkConstMethod "isNull" [] TBool
  , mkConstMethod "isNumber" [] TBool
  , mkConstMethod "isPrint" [] TBool
  , mkConstMethod "isPunct" [] TBool
  , mkConstMethod "isSpace" [] TBool
  , mkConstMethod "isSymbol" [] TBool
  , mkConstMethod "isTitleCase" [] TBool
  , mkConstMethod "isUpper" [] TBool
  , mkConstMethod "joining" [] $ TEnum e_Joining
  , mkConstMethod "mirroredChar" [] $ TObj c_QChar
  , mkConstMethod "row" [] TUChar
  , mkConstMethod "toAscii" [] TChar
  , mkConstMethod "toCaseFolded" [] $ TObj c_QChar
  , mkConstMethod "toLatin1" [] TChar
  , mkConstMethod "toLower" [] $ TObj c_QChar
  , mkConstMethod "toTitleCase" [] $ TObj c_QChar
  , mkConstMethod "toUpper" [] $ TObj c_QChar
  , mkConstMethod' "unicode" "unicode" [] TUShort
  , mkMethod' "unicode" "unicodeRef" [] $ TRef TUShort
  , mkConstMethod "unicodeVersion" [] $ TEnum e_UnicodeVersion
  ]

e_Category =
  makeQtEnum (ident1 "QChar" "Category")
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
  makeQtEnum (ident1 "QChar" "Decomposition")
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
  makeQtEnum (ident1 "QChar" "Direction")
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

e_Joining =
  makeQtEnum (ident1 "QChar" "Joining")
  [ (3, ["center"])
  , (1, ["dual"])
  , (0, ["other", "joining"])
  , (2, ["right"])
  ]

e_SpecialCharacter =
  makeQtEnum (ident1 "QChar" "SpecialCharacter")
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
  makeQtEnum (ident1 "QChar" "UnicodeVersion")
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
