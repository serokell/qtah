{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.Core.QMargins (
  qtModule,
  c_QMargins,
  ) where

import qualified Data.Set as S
import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsType (HsTyApp, HsTyCon),
  )

{-# ANN module "HLint: ignore Use camelCase" #-}

qtModule = makeQtModuleForClass c_QMargins []

this = c_QMargins
#include "../Mk.hs.inc"

c_QMargins =
  addReqIncludes [includeStd "QMargins"] $
  classModifyEncoding
  (\c -> c { classCppCType = Just $ TPtr TInt
           , classCppDecoder = Just $ CppCoderFn (ident "qMarginsDecode") $
                               reqInclude $ includeLocal "encode.hpp"
           , classCppEncoder = Just $ CppCoderFn (ident "qMarginsEncode") $
                               reqInclude $ includeLocal "encode.hpp"
           , classHaskellType =
             Just $ HaskellEncoding
             { haskellEncodingType = HsTyCon $ UnQual $ HsIdent "HMargins.HMargins"
             , haskellEncodingCType = HsTyApp (HsTyCon $ UnQual $ HsIdent "QtahF.Ptr") $
                                      HsTyCon $ UnQual $ HsIdent "QtahFC.CInt"
             , haskellEncodingDecoder = "HMargins.decodeInternal"
             , haskellEncodingEncoder = "HMargins.encodeInternal"
             , haskellEncodingTypeImports =
               S.singleton "qualified Graphics.UI.Qtah.H.HMargins as HMargins"
             , haskellEncodingCTypeImports =
               S.fromList
               [ "qualified Foreign as QtahF"
               , "qualified Foreign.C as QtahFC"
               ]
             , haskellEncodingFnImports =
               S.singleton "qualified Graphics.UI.Qtah.H.HMargins as HMargins"
             }
           }) $
  makeClass (ident "QMargins") Nothing []
  [ _mkCtor "newNull" []
  , _mkCtor "new" [TInt, TInt, TInt, TInt]
  ]
  [ _mkConstMethod "bottom" [] TInt
  , _mkConstMethod "isNull" [] TBool
  , _mkConstMethod "left" [] TInt
  , _mkConstMethod "right" [] TInt
  , _mkMethod "setBottom" [TInt] TVoid
  , _mkMethod "setLeft" [TInt] TVoid
  , _mkMethod "setRight" [TInt] TVoid
  , _mkMethod "setTop" [TInt] TVoid
  , _mkConstMethod "top" [] TInt
  ]
