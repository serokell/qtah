{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.Core.QMargins (
  cppopModule,
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
#include "../Mk.hs.inc"

cppopModule =
  modifyModule' (makeModule "qmargins" "gen_qmargins.hpp" "gen_qmargins.cpp") $ do
    addModuleHaskellName ["Core", "QMargins"]
    addModuleExports exports

qtModule = makeQtModule "Core.QMargins" $ map QtExport exports

exports = [ExportClass c_QMargins]

this = c_QMargins

c_QMargins =
  addReqIncludes [includeStd "QMargins"] $
  classModifyEncoding
  (\c -> c { classCppCType = Just $ TPtr TInt
           , classCppDecoder = Just $ CppCoderFn (ident "qMarginsDecode") $
                               reqInclude $ includeLocal "encode.hpp"
           , classCppEncoder = Just $ CppCoderFn (ident "qMarginsEncode") $
                               reqInclude $ includeLocal "encode.hpp"
           , classHaskellType =
             Just HaskellEncoding
             { haskellEncodingType = HsTyCon $ UnQual $ HsIdent "HMargins.HMargins"
             , haskellEncodingCType = HsTyApp (HsTyCon $ UnQual $ HsIdent "QtahF.Ptr") $
                                      HsTyCon $ UnQual $ HsIdent "QtahFC.CInt"
             , haskellEncodingDecoder = "HMargins.decodeInternal"
             , haskellEncodingEncoder = "HMargins.encodeInternal"
             , haskellEncodingTypeImports =
               S.singleton "qualified Graphics.UI.Qtah.Core.HMargins as HMargins"
             , haskellEncodingCTypeImports =
               S.fromList
               [ "qualified Foreign as QtahF"
               , "qualified Foreign.C as QtahFC"
               ]
             , haskellEncodingFnImports =
               S.singleton "qualified Graphics.UI.Qtah.Core.HMargins as HMargins"
             }
           }) $
  makeClass (ident "QMargins") Nothing []
  [ _mkCtor "newNull" []
  , _mkCtor "new" [TInt, TInt, TInt, TInt]
  ] $
  [ _mkConstMethod "isNull" [] TBool
  ] ++
  _props
  [ _mkProp "bottom" TInt
  , _mkProp "left" TInt
  , _mkProp "right" TInt
  , _mkProp "top" TInt
  ]
