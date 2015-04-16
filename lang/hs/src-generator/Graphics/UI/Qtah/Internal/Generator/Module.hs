module Graphics.UI.Qtah.Internal.Generator.Module (
  generateModule,
  ) where

import Control.Arrow ((&&&))
import Control.Monad (forM_, when)
import Data.List (isPrefixOf)
import Foreign.Cppop.Generator.Language.Haskell.General (
  execGenerator,
  indent,
  ln,
  sayLn,
  saysLn,
  toHsCastMethodName,
  toHsClassName,
  toHsClassNullName,
  toHsDataTypeName,
  toHsFnName,
  )
import Foreign.Cppop.Generator.Spec (
  Class,
  Constness (Const, Nonconst),
  Ctor,
  ExtName,
  Function,
  Method,
  classCtors,
  classExtName,
  classMethods,
  ctorExtName,
  fnExtName,
  fromExtName,
  methodExtName,
  toExtName,
  )
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Generator.Signal (
  moduleNameToSignalModuleName,
  toSignalBindingName,
  )
import System.Exit (exitFailure)
import System.FilePath ((</>), (<.>), pathSeparator)

generateModule :: FilePath -> String -> String -> QtModule -> IO ()
generateModule srcDir baseModuleName foreignModuleName qtModule = do
  let fullModuleName = moduleNameAppend baseModuleName $ qtModuleSubname qtModule
      qtExports = qtModuleExports qtModule

  let generation = execGenerator $ do
        sayLn "{-# LANGUAGE NoMonomorphismRestriction #-}"
        sayLn "---------- GENERATED FILE, EDITS WILL BE LOST ----------"
        ln
        saysLn ["module ", fullModuleName, " ("]
        indent $ do
          forM_ (concatMap getReexportNames qtExports) $ \name ->
            saysLn [name, ","]
          sayLn ") where"

        -- Import functions and classes from the main module.
        ln
        saysLn ["import ", foreignModuleName, " ("]
        indent $ do
          forM_ (concatMap getImportSpecsFromMainModule qtExports) $ \spec ->
            saysLn [spec, ","]
          sayLn ")"

        -- Import signals from the signal module.
        saysLn ["import ", moduleNameToSignalModuleName foreignModuleName, " ("]
        indent $ do
          forM_ (concatMap getImportSpecsFromSignalModule qtExports) $ \spec ->
            saysLn [spec, ","]
          sayLn ")"
        sayLn "import Prelude ()"

        -- Generate bindings with abbreviated names.
        ln
        forM_ (concatMap getBindings qtExports) $ \(name, value) ->
          -- Functions aren't renamed, and declaring "x = x" would be a problem!
          when (name /= value) $
          saysLn [name, " = ", value]

  case generation of
    Left errorMsg -> do
      putStrLn $ "Error generating Qt modules: " ++ errorMsg
      exitFailure
    Right source -> do
      let path =
            srcDir </>
            map (\c -> if c == '.' then pathSeparator else c) fullModuleName <.>
            "hs"
      writeFile path source

getReexportNames :: QtExport -> [String]
getReexportNames qtExport = case qtExport of
  QtExportFn fn -> [getFnReexportName fn]
  QtExportClass qtCls ->
    let cls = qtClassClass qtCls
    in toHsClassName Const cls :
       toHsClassName Nonconst cls :
       toHsDataTypeName Const cls :
       toHsDataTypeName Nonconst cls :
       classConstCastReexportName :
       classCastReexportName :
       classNullReexportName :
       concat [ map (getCtorReexportName cls) $ classCtors cls
              , map (getMethodReexportName cls) $ classMethods cls
              , map (getSignalReexportName cls) $ qtClassSignals qtCls
              ]
  QtExportCallback _ -> []

getFnReexportName :: Function -> String
getFnReexportName = getFnImportName

classCastReexportName :: String
classCastReexportName = "cast"

classConstCastReexportName :: String
classConstCastReexportName = "castConst"

classNullReexportName :: String
classNullReexportName = "null"

getCtorReexportName :: Class -> Ctor -> String
getCtorReexportName cls = toBindingNameWithoutClassPrefix cls . ctorExtName

getMethodReexportName :: Class -> Method -> String
getMethodReexportName cls = toBindingNameWithoutClassPrefix cls . methodExtName

getSignalReexportName :: Class -> Signal -> String
getSignalReexportName cls =
  (++ "Signal") . toBindingNameWithoutClassPrefix cls . signalExtName

getImportSpecsFromMainModule :: QtExport -> [String]
getImportSpecsFromMainModule qtExport = case qtExport of
  QtExportFn fn -> [getFnImportName fn]
  QtExportClass qtCls ->
    let cls = qtClassClass qtCls
    in (toHsClassName Const cls ++ " (..)") :
       (toHsClassName Nonconst cls ++ " (..)") :
       toHsDataTypeName Const cls :
       toHsDataTypeName Nonconst cls :
       toHsClassNullName cls :
       concat [ map (toHsFnName . ctorExtName) $ classCtors cls
              , map (toHsFnName . methodExtName) $ classMethods cls
              ]
  QtExportCallback _ -> []

getImportSpecsFromSignalModule :: QtExport -> [String]
getImportSpecsFromSignalModule qtExport = case qtExport of
  QtExportFn _ -> []
  QtExportClass qtCls -> map toSignalBindingName $ qtClassSignals qtCls
  QtExportCallback _ -> []

getFnImportName :: Function -> String
getFnImportName = toHsFnName . fnExtName

getBindings :: QtExport -> [(String, String)]
getBindings qtExport = case qtExport of
  QtExportFn fn -> [(getFnReexportName fn, getFnImportName fn)]
  QtExportClass qtCls ->
    let cls = qtClassClass qtCls
    in (classConstCastReexportName, toHsCastMethodName Const cls) :
       (classCastReexportName, toHsCastMethodName Nonconst cls) :
       (classNullReexportName, toHsClassNullName cls) :
       concat [ map (getCtorReexportName cls &&& toHsFnName . ctorExtName) $
                classCtors cls
              , map (getMethodReexportName cls &&& toHsFnName . methodExtName) $
                classMethods cls
              , map (getSignalReexportName cls &&& toSignalBindingName) $
                qtClassSignals qtCls
              ]
  QtExportCallback _ -> []

toBindingNameWithoutClassPrefix :: Class -> ExtName -> String
toBindingNameWithoutClassPrefix cls name =
  toHsFnName $ toExtName $
  dropPrefix (fromExtName (classExtName cls) ++ "_") $
  fromExtName name

dropPrefix :: String -> String -> String
dropPrefix prefix str =
  if prefix `isPrefixOf` str
  then drop (length prefix) str
  else str
