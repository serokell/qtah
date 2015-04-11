module Graphics.UI.Qtah.Internal.Interface.Callbacks where

import Foreign.Cppop.Generator.Spec
import Foreign.Cppop.Generator.Std

cb_IntIntVoid =
  Callback (toExtName "CallbackIntIntVoid")
  [TInt, TInt] TVoid

cb_IntVoid =
  Callback (toExtName "CallbackIntVoid")
  [TInt] TVoid

cb_StringVoid =
  Callback (toExtName "CallbackStringVoid")
  [TObj cls_std__string] TVoid

f_testIntCallback =
  Function (ident "testIntCallback") (toExtName "testIntCallback") Nonpure
  [TCallback cb_IntVoid] TVoid

f_testStringCallback =
  Function (ident "testStringCallback") (toExtName "testStringCallback") Nonpure
  [TCallback cb_StringVoid] TVoid
