import Data.Char (toUpper)

{-# ANN module "HLint: ignore Use camelCase" #-}

_mkCtor :: String -> [Type] -> Ctor
_mkCtor name =
  makeCtor (toExtName $ fromExtName (classExtName this) ++ "_" ++ name)

_mkMethod :: String -> [Type] -> Type -> Method
_mkMethod name =
  makeMethod name (toExtName $ fromExtName (classExtName this) ++ "_" ++ name)
  MNormal Nonpure

_mkMethod' :: String -> String -> [Type] -> Type -> Method
_mkMethod' cName hsName =
  makeMethod cName (toExtName $ fromExtName (classExtName this) ++ "_" ++ hsName)
  MNormal Nonpure

_mkConstMethod :: String -> [Type] -> Type -> Method
_mkConstMethod name =
  makeMethod name (toExtName $ fromExtName (classExtName this) ++ "_" ++ name)
  MConst Nonpure

_mkConstMethod' :: String -> String -> [Type] -> Type -> Method
_mkConstMethod' cName hsName =
  makeMethod cName (toExtName $ fromExtName (classExtName this) ++ "_" ++ hsName)
  MConst Nonpure

_mkStaticMethod :: String -> [Type] -> Type -> Method
_mkStaticMethod name =
  makeMethod name (toExtName $ fromExtName (classExtName this) ++ "_" ++ name)
  MStatic Nonpure

_mkStaticMethod' :: String -> String -> [Type] -> Type -> Method
_mkStaticMethod' cName hsName =
  makeMethod cName (toExtName $ fromExtName (classExtName this) ++ "_" ++ hsName)
  MStatic Nonpure

_mkSignal :: String -> Class -> Signal
_mkSignal = makeSignal this

_mkProp :: String -> Type -> [Method]
_mkProp name t =
  let c:cs = name
      setName = 's' : 'e' : 't' : toUpper c : cs
  in [ _mkConstMethod name [] t
     , _mkMethod setName [t] TVoid
     ]

_mkStaticProp :: String -> Type -> [Method]
_mkStaticProp name t =
  let c:cs = name
      setName = 's' : 'e' : 't' : toUpper c : cs
  in [ _mkStaticMethod name [] t
     , _mkStaticMethod setName [t] TVoid
     ]

_mkBoolIsProp :: String -> [Method]
_mkBoolIsProp name =
  let c:cs = name
      name' = toUpper c : cs
      isName = 'i':'s':name'
      setName = 's':'e':'t':name'
  in [ _mkConstMethod isName [] TBool
     , _mkMethod setName [TBool] TVoid
     ]

_mkBoolHasProp :: String -> [Method]
_mkBoolHasProp name =
  let c:cs = name
      name' = toUpper c : cs
      hasName = 'h':'a':'s':name'
      setName = 's':'e':'t':name'
  in [ _mkConstMethod hasName [] TBool
     , _mkMethod setName [TBool] TVoid
     ]

_props :: [[Method]] -> [Method]
_props = concat
