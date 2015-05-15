{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.Core.QPoint (
  qtModule,
  c_QPoint,
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

qtModule = makeQtModuleForClass c_QPoint []

this = c_QPoint
#include "../Mk.hs.inc"

c_QPoint =
  addReqIncludes [includeStd "QPoint"] $
  classModifyEncoding
  (\c -> c { classCppCType = Just $ TPtr TInt
           , classCppDecoder = Just $ CppCoderFn (ident "qPointDecode") $
                               reqInclude $ includeLocal "encode.hpp"
           , classCppEncoder = Just $ CppCoderFn (ident "qPointEncode") $
                               reqInclude $ includeLocal "encode.hpp"
           , classHaskellType =
             Just HaskellEncoding
             { haskellEncodingType = HsTyCon $ UnQual $ HsIdent "HPoint.HPoint"
             , haskellEncodingCType = HsTyApp (HsTyCon $ UnQual $ HsIdent "QtahF.Ptr") $
                                      HsTyCon $ UnQual $ HsIdent "QtahFC.CInt"
             , haskellEncodingDecoder = "HPoint.decodeInternal"
             , haskellEncodingEncoder = "HPoint.encodeInternal"
             , haskellEncodingTypeImports =
               S.singleton "qualified Graphics.UI.Qtah.H.HPoint as HPoint"
             , haskellEncodingCTypeImports =
               S.fromList
               [ "qualified Foreign as QtahF"
               , "qualified Foreign.C as QtahFC"
               ]
             , haskellEncodingFnImports =
               S.singleton "qualified Graphics.UI.Qtah.H.HPoint as HPoint"
             }
           }) $
  makeClass (ident "QPoint") Nothing []
  [ _mkCtor "newNull" []
  , _mkCtor "new" [TInt, TInt]
  ]
  [ _mkConstMethod "isNull" [] TBool
  , _mkConstMethod "manhattanLength" [] TInt
  , _mkMethod "setX" [TInt] TVoid
  , _mkMethod "setY" [TInt] TVoid
  , _mkConstMethod "x" [] TInt
  , _mkConstMethod "y" [] TInt
  ]
