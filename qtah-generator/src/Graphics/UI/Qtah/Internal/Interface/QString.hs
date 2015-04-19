module Graphics.UI.Qtah.Internal.Interface.QString (
  mod_QString,
  c_QString,
  ) where

import Foreign.Cppop.Generator.Spec
import Foreign.Cppop.Generator.Std
import Graphics.UI.Qtah.Internal.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

mod_QString =
  makeQtModule "QString" []
  [ QtExportClass qtc_QString ]

c_QString = qtClassClass qtc_QString

qtc_QString =
  makeQtClass' [] $
  classModifyEncoding
  (\c -> c { classCppCType = Just $ TPtr TChar
           , classCppDecoder = Just $ CppCoderFn $ ident "QString"
           , classCppEncoder = Just $ CppCoderExpr [Just "strdup(", Nothing, Just ".toStdString().c_str())"]
           }) $
  classCopyEncodingFrom c_std__string $
  makeClass (ident "QString") Nothing [] [] []
