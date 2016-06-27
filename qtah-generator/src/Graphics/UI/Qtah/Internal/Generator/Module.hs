-- This file is part of Qtah.
--
-- Copyright 2015-2016 Bryan Gardiner <bog@khumba.net>
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

module Graphics.UI.Qtah.Internal.Generator.Module (
  generateModule,
  ) where

import Control.Monad (forM_, unless, when)
#if MIN_VERSION_mtl(2,2,1)
import Control.Monad.Except (throwError)
#else
import Control.Monad.Error (throwError)
#endif
import Data.List (find, intercalate, intersperse, isPrefixOf, sort)
import Data.Maybe (isJust)
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mconcat)
#endif
import Foreign.Hoppy.Generator.Language.Cpp (execChunkWriter, sayType)
import Foreign.Hoppy.Generator.Language.Haskell (
  Generator,
  HsTypeSide (HsHsSide),
  addExport,
  addExports,
  addImports,
  cppTypeToHsTypeAndUse,
  execGenerator,
  getClassHaskellConversion,
  importHsModuleForExtName,
  indent,
  inFunction,
  ln,
  prettyPrint,
  renderPartial,
  sayLn,
  saysLn,
  toHsBitspaceClassName,
  toHsBitspaceToNumName,
  toHsBitspaceTypeName,
  toHsBitspaceValueName,
  toHsCastMethodName,
  toHsDataTypeName,
  toHsDownCastMethodName,
  toHsEnumTypeName,
  toHsFnName,
  toHsPtrClassName,
  toHsValueClassName,
  )
import Foreign.Hoppy.Generator.Spec (
  Class,
  Constness (Const, Nonconst),
  Ctor,
  Export (ExportBitspace, ExportCallback, ExportClass, ExportEnum, ExportFn, ExportVariable),
  ExtName,
  FnName (FnName),
  Function,
  Interface,
  Method,
  MethodImpl (RealMethod),
  Type (TCallback, TObj),
  bitspaceExtName,
  bitspaceValueNames,
  callbackParams,
  classCtors,
  classExtName,
  classMethods,
  ctorExtName,
  ctorParams,
  enumExtName,
  fnExtName,
  fromExtName,
  getClassyExtName,
  hsImport1,
  hsImports,
  methodExtName,
  methodImpl,
  toExtName,
  varExtName,
  varGetterExtName,
  varIsConst,
  varSetterExtName,
  )
import Graphics.UI.Qtah.Internal.Generator.Common (fromMaybeM, writeFileIfDifferent)
import Graphics.UI.Qtah.Internal.Generator.Types (
  QtExport (QtExport, QtExportEvent, QtExportFnRenamed, QtExportSignal, QtExportSpecials),
  QtModule,
  Signal,
  moduleNameAppend,
  qtModulePath,
  qtModuleQtExports,
  signalClass,
  signalCName,
  signalHaskellName,
  signalListenerClass,
  )
import Graphics.UI.Qtah.Internal.Interface.Imports
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsQualType (HsQualType),
  HsType (HsTyApp, HsTyCon, HsTyFun, HsTyVar),
  )
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitFailure)
import System.FilePath ((</>), (<.>), pathSeparator, takeDirectory)

generateModule :: Interface -> FilePath -> String -> QtModule -> IO ()
generateModule iface srcDir baseModuleName qtModule = do
  let fullModuleName = moduleNameAppend baseModuleName $ intercalate "." $ qtModulePath qtModule
      qtExports = qtModuleQtExports qtModule

  let generation =
        fmap (("{-# LANGUAGE NoMonomorphismRestriction #-}\n\n" ++) . renderPartial) $
        execGenerator iface fullModuleName $ do
          -- As in generated Hoppy bindings, avoid non-qualified Prelude uses in
          -- generated code here.
          addImports $ hsImports "Prelude" []

          -- Generate bindings for all of the exports.
          mapM_ (sayQtExport $ qtModulePath qtModule) qtExports

  case generation of
    Left errorMsg -> do
      putStrLn $ "Error generating Qt modules: " ++ errorMsg
      exitFailure
    Right source -> do
      let path =
            srcDir </>
            map (\c -> if c == '.' then pathSeparator else c) fullModuleName <.>
            "hs"
      createDirectoryIfMissing True $ takeDirectory path
      writeFileIfDifferent path source

getFnReexportName :: Function -> String
getFnReexportName = getFnImportName

classUpCastReexportName :: String
classUpCastReexportName = "cast"

classUpCastConstReexportName :: String
classUpCastConstReexportName = "castConst"

classDownCastReexportName :: String
classDownCastReexportName = "downCast"

classDownCastConstReexportName :: String
classDownCastConstReexportName = "downCastConst"

classEncodeReexportName :: String
classEncodeReexportName = "encode"

classDecodeReexportName :: String
classDecodeReexportName = "decode"

getCtorReexportName :: Class -> Ctor -> String
getCtorReexportName cls = toBindingNameWithoutClassPrefix cls . ctorExtName

getMethodReexportName :: Class -> Method -> String
getMethodReexportName cls = toBindingNameWithoutClassPrefix cls . methodExtName

-- | Qtah uses @ClassName_innerName@ 'ExtName's for things in classes.  This
-- function strips the @ClassName_@ prefix off of an 'ExtName', if present, and
-- converts it to a function name.
toBindingNameWithoutClassPrefix :: Class -> ExtName -> String
toBindingNameWithoutClassPrefix cls name =
  toHsFnName $ toExtName $
  dropPrefix (fromExtName (classExtName cls) ++ "_") $
  fromExtName name

-- | @dropPrefix prefix str@ strips @prefix@ from @str@ if @str@ starts with
-- @prefix@, and returns @str@ unmodified otherwise.
dropPrefix :: String -> String -> String
dropPrefix prefix str =
  if prefix `isPrefixOf` str
  then drop (length prefix) str
  else str

getFnImportName :: Function -> String
getFnImportName = toHsFnName . fnExtName

sayClassEncodingFnReexports :: Class -> Generator ()
sayClassEncodingFnReexports cls = inFunction "sayClassEncodingFnReexports" $
  when (classIsConvertible cls) $ do
    -- Generated encode and decode functions require some things from Hoppy
    -- support and the Prelude.
    addImports $ mconcat [importForPrelude, importForRuntime]

    hsHsType <- cppTypeToHsTypeAndUse HsHsSide (TObj cls)
    let constPtrClassName = toHsPtrClassName Const cls
        dataTypeName = toHsDataTypeName Nonconst cls
        ptrHsType = HsTyCon $ UnQual $ HsIdent dataTypeName
        thisTyVar = HsTyVar $ HsIdent "this"
        encodeFnType = HsTyFun hsHsType $ HsTyApp (HsTyCon $ UnQual $ HsIdent "QtahP.IO") ptrHsType
        decodeFnType = HsQualType [(UnQual $ HsIdent constPtrClassName, [thisTyVar])] $
                       HsTyFun thisTyVar $
                       HsTyApp (HsTyCon $ UnQual $ HsIdent "QtahP.IO") hsHsType
    ln
    saysLn [classEncodeReexportName, " :: ", prettyPrint encodeFnType]
    saysLn [classEncodeReexportName, " = QtahFHR.encodeAs (QtahP.undefined :: ", dataTypeName, ")"]
    ln
    saysLn [classDecodeReexportName, " :: ", prettyPrint decodeFnType]
    saysLn [classDecodeReexportName, " = QtahFHR.decode QtahP.. ", toHsCastMethodName Const cls]

sayQtExport :: [String] -> QtExport -> Generator ()
sayQtExport path qtExport = case qtExport of
  QtExport (ExportVariable v) -> do
    importHsModuleForExtName $ varExtName v
    addExport $ toHsFnName $ varGetterExtName v
    unless (varIsConst v) $ addExport $ toHsFnName $ varSetterExtName v

  QtExport (ExportEnum e) -> do
    importHsModuleForExtName $ enumExtName e
    let spec = toHsEnumTypeName e ++ " (..)"
    addExport spec

  QtExport (ExportBitspace b) -> do
    importHsModuleForExtName $ bitspaceExtName b
    addExport $ toHsBitspaceTypeName b
    addExport $ toHsBitspaceToNumName b
    addExport $ toHsBitspaceClassName b ++ " (..)"
    forM_ (bitspaceValueNames b) $ \(_, valueName) ->
      addExport $ toHsBitspaceValueName b valueName

  QtExport (ExportFn fn) -> do
    importHsModuleForExtName $ fnExtName fn
    addExport $ getFnReexportName fn

  QtExportFnRenamed fn rename -> do
    importHsModuleForExtName $ fnExtName fn
    addExport rename
    sayBind rename $ getFnImportName fn

  QtExport (ExportCallback _) -> return ()

  QtExport (ExportClass cls) -> sayExportClass cls

  QtExportSignal sig -> sayExportSignal sig

  QtExportEvent cls -> do
    sayExportClass cls

    let typeName = toHsDataTypeName Nonconst cls
    addImports $ mconcat [hsImport1 "Prelude" "($)",
                          importForEvent]
    ln
    saysLn ["instance QtahEvent.Event ", typeName, " where"]
    indent $ do
      sayLn "onEvent receiver' handler' = QtahEvent.onAnyEvent receiver' $ \\_ qevent' ->"
      indent $
        if path == ["Core", "QEvent"]
        then sayLn "handler' qevent'"
        else do
          addImports $ mconcat [hsImport1 "Prelude" "(==)",
                                importForPrelude,
                                importForRuntime]
          saysLn ["let event' = ", classDownCastReexportName, " qevent'"]
          sayLn "in if event' == QtahFHR.nullptr then QtahP.return QtahP.False else handler' event'"

  QtExportSpecials -> do
    -- Generate a type synonym for qreal.
    addImports importForForeignC
    addExport "QReal"
    ln
    saysLn ["type QReal = ", if QREAL_FLOAT then "QtahFC.CFloat" else "QtahFC.CDouble"]

sayExportClass :: Class -> Generator ()
sayExportClass cls = do
  importHsModuleForExtName $ classExtName cls
  addExports $
    (toHsValueClassName cls ++ " (..)") :
    (toHsPtrClassName Const cls ++ " (..)") :
    (toHsPtrClassName Nonconst cls ++ " (..)") :
    toHsDataTypeName Const cls :
    toHsDataTypeName Nonconst cls :
    classUpCastConstReexportName :
    classUpCastReexportName :
    classDownCastConstReexportName :
    classDownCastReexportName :
    concat [ if classIsConvertible cls
             then [classEncodeReexportName, classDecodeReexportName]
             else []
           , sort $ map (getCtorReexportName cls) $ classCtors cls
           , sort $ map (getMethodReexportName cls) $ classMethods cls
           ]

  ln
  sayBind classUpCastConstReexportName $ toHsCastMethodName Const cls
  sayBind classUpCastReexportName $ toHsCastMethodName Nonconst cls
  sayBind classDownCastConstReexportName $ toHsDownCastMethodName Const cls
  sayBind classDownCastReexportName $ toHsDownCastMethodName Nonconst cls
  sayClassEncodingFnReexports cls
  forM_ (classCtors cls) $ \ctor ->
    sayBind (getCtorReexportName cls ctor) $ toHsFnName $ getClassyExtName cls ctor
  forM_ (classMethods cls) $ \method ->
    sayBind (getMethodReexportName cls method) $ toHsFnName $ getClassyExtName cls method

-- | Generates and exports a @Signal@ definition.  We create the signal from
-- scratch in this module, rather than reexporting it from somewhere else.
sayExportSignal :: Signal -> Generator ()
sayExportSignal signal = inFunction "sayExportSignal" $ do
  addImports importForSignal

  let name = signalCName signal
      cls = signalClass signal
      ptrClassName = toHsPtrClassName Nonconst cls
      varName = toSignalBindingName signal
  addExport varName

  let listenerClass = signalListenerClass signal
  importHsModuleForExtName $ classExtName listenerClass
  -- Find the listener constructor that only takes a callback.
  listenerCtor <-
    fromMaybeM (throwError $ concat
                ["Couldn't find an appropriate ",
                show (fromExtName $ classExtName listenerClass),
                " constructor for signal ", show name]) $
    flip find (classCtors listenerClass) $ \ctor -> case ctorParams ctor of
      [TCallback {}] -> True
      _ -> False
  let [callbackType@(TCallback callback)] = ctorParams listenerCtor
      paramTypes = callbackParams callback

  -- Also find the 'connectListener' method.
  listenerConnectMethod <-
    fromMaybeM (throwError $ concat
                ["Couldn't find the connectListener method in ",
                 show listenerClass, " for signal ", show name]) $
    find ((RealMethod (FnName "connectListener") ==) . methodImpl) $ classMethods listenerClass

  callbackHsType <- cppTypeToHsTypeAndUse HsHsSide callbackType

  let varType = HsQualType [(UnQual $ HsIdent ptrClassName, [HsTyVar $ HsIdent "object"])] $
                HsTyApp (HsTyApp (HsTyCon $ UnQual $ HsIdent "QtahSignal.Signal") $
                         HsTyVar $ HsIdent "object")
                callbackHsType
  ln
  saysLn [varName, " :: ", prettyPrint varType]
  saysLn [varName, " = QtahSignal.Signal"]
  indent $ do
    sayLn "{ QtahSignal.internalConnectSignal = \\object' fn' -> do"
    indent $ do
      saysLn ["listener' <- ", toHsFnName $ getClassyExtName listenerClass listenerCtor, " fn'"]
      saysLn [toHsFnName $ getClassyExtName listenerClass listenerConnectMethod,
              " listener' object' ", show (toSignalConnectName signal paramTypes)]
    sayLn "}"

sayBind :: String -> String -> Generator ()
sayBind name value = saysLn [name, " = ", value]

toSignalBindingName :: Signal -> String
toSignalBindingName = (++ "Signal") . signalHaskellName

toSignalConnectName :: Signal -> [Type] -> String
toSignalConnectName signal paramTypes =
  concat $
  "2" :  -- This is a magic code added by the SIGNAL() macro.
  signalCName signal :
  "(" :
  intersperse "," (map (execChunkWriter . sayType Nothing) paramTypes) ++
  [")"]

-- | Returns true iff a given class is convertible to/from a Haskell
-- type with @encode@ and @decode@.
classIsConvertible :: Class -> Bool
classIsConvertible = isJust . getClassHaskellConversion
