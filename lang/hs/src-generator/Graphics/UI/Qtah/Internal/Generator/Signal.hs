module Graphics.UI.Qtah.Internal.Generator.Signal (
  generateSignals,
  moduleNameToSignalModuleName,
  ) where

import Data.Foldable (forM_)
import Data.List (find, intersperse)
import Graphics.UI.Qtah.Internal.Generator.Moc
import Foreign.Cppop.Common (fromMaybeM)
import Foreign.Cppop.Generator.Language.Cpp.General (execChunkWriter, sayType)
import Foreign.Cppop.Generator.Language.Haskell.General (
  Generator,
  HsTypeSide (HsHsSide),
  abort,
  cppTypeToHsType,
  indent,
  ln,
  sayLn,
  sayQualifiedImports,
  saysLn,
  toHsClassName,
  toHsFnName,
  )
import Foreign.Cppop.Generator.Spec (
  Constness (Nonconst),
  Type (TCallback),
  callbackParams,
  classCtors,
  classExtName,
  classMethods,
  ctorExtName,
  ctorParams,
  fromExtName,
  methodCName,
  methodExtName,
  )
import Language.Haskell.Pretty (prettyPrint)
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsQualType (HsQualType),
  HsType (HsTyApp, HsTyCon, HsTyVar),
  )

moduleNameToSignalModuleName :: String -> String
moduleNameToSignalModuleName = (++ "Signal")

generateSignals :: String -> [QtClass] -> Generator ()
generateSignals baseModuleName classes = do
  let moduleName = moduleNameToSignalModuleName baseModuleName
  sayLn "---------- GENERATED FILE, EDITS WILL BE LOST ----------"
  ln
  -- TODO Use the Haskell generator's getModuleName function.
  saysLn ["module Foreign.Cppop.Generated.", moduleName, " ("]
  indent $ do
    forAllSignals classes $ \signal ->
      saysLn [toSignalVarName signal, ","]
    sayLn ") where"
  ln
  sayQualifiedImports
  ln
  saysLn ["import Foreign.Cppop.Generated.", baseModuleName]
  sayLn "import qualified Graphics.UI.Qtah.Signal as QtahSignal"
  forAllSignals classes $ \signal -> do
    let name = signalExtName signal
        className = toHsClassName Nonconst $ qtClassClass $ signalClass signal
        varName = toSignalVarName signal

    let listenerClass = signalListenerClass signal
    -- Find the listener constructor that only takes a callback.
    listenerCtor <-
      fromMaybeM (abort $ "generateSignals: Couldn't find an appropriate " ++
                  show (fromExtName $ classExtName listenerClass) ++
                  " constructor for signal " ++ show (fromExtName name) ++ ".") $
      flip find (classCtors listenerClass) $ \ctor -> case ctorParams ctor of
        [TCallback {}] -> True
        _ -> False
    let [callbackType@(TCallback callback)] = ctorParams listenerCtor
        paramTypes = callbackParams callback

    -- Also find the 'connectListener' method.
    listenerConnectMethod <-
      fromMaybeM (abort $ "generateSignals: Couldn't find the connectListener method in class " ++
                  show (fromExtName $ classExtName listenerClass) ++
                  " for signal " ++ show (fromExtName name) ++ ".") $
      find (("connectListener" ==) . methodCName) $ classMethods listenerClass

    callbackHsType <-
      fromMaybeM (abort $
                  "generateSignals: Can't generate Haskell callback type for signal " ++
                  show (fromExtName name) ++ ".") $
      cppTypeToHsType HsHsSide callbackType

    let varType = HsQualType [(UnQual $ HsIdent className, [HsTyVar $ HsIdent "object"])] $
                  HsTyApp (HsTyApp (HsTyCon $ UnQual $ HsIdent "QtahSignal.Signal") $
                           HsTyVar $ HsIdent "object")
                  callbackHsType
    ln
    saysLn [varName, " :: ", prettyPrint varType]
    saysLn [varName, " = QtahSignal.Signal"]
    indent $ do
      sayLn "{ QtahSignal.connectSignal = \\object' fn' -> do"
      indent $ do
        saysLn ["listener' <- ", toHsFnName $ ctorExtName listenerCtor, " fn'"]
        saysLn [toHsFnName $ methodExtName listenerConnectMethod, " listener' object' ",
                show (toSignalConnectName signal paramTypes)]
      sayLn "}"

forAllSignals :: Monad m => [QtClass] -> (Signal -> m ()) -> m ()
forAllSignals classes action =
  forM_ classes $ \qtCls ->
    forM_ (qtClassSignals qtCls) action

toSignalVarName :: Signal -> String
toSignalVarName = (++ "_signal") . toHsFnName . signalExtName

toSignalConnectName :: Signal -> [Type] -> String
toSignalConnectName signal paramTypes =
  concat $
  "2" :  -- This is a magic code added by the SIGNAL() macro.
  signalCName signal :
  "(" :
  intersperse "," (map (execChunkWriter . sayType Nothing) paramTypes) ++
  [")"]
