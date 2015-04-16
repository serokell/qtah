module Main where

import Data.Foldable (forM_)
import Data.Maybe (mapMaybe)
import Foreign.Cppop.Common (maybeFail)
import Foreign.Cppop.Generator.Language.Haskell.General (execGenerator)
import Foreign.Cppop.Generator.Main (Action (GenHaskell), run)
import Foreign.Cppop.Generator.Spec (
  Include,
  Interface,
  Export (ExportCallback, ExportClass),
  includeLocal,
  includeStd,
  interface,
  )
import Foreign.Cppop.Generator.Std (c_std__string)
import Graphics.UI.Qtah.Internal.Generator.Module
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Generator.Signal
import Graphics.UI.Qtah.Internal.Interface.Callback (allCallbacks)
import Graphics.UI.Qtah.Internal.Interface.Listener (allListeners)
import Graphics.UI.Qtah.Internal.Interface.QAbstractButton
import Graphics.UI.Qtah.Internal.Interface.QAbstractScrollArea
import Graphics.UI.Qtah.Internal.Interface.QApplication
import Graphics.UI.Qtah.Internal.Interface.QBoxLayout
import Graphics.UI.Qtah.Internal.Interface.QCoreApplication
import Graphics.UI.Qtah.Internal.Interface.QFrame
import Graphics.UI.Qtah.Internal.Interface.QHBoxLayout
import Graphics.UI.Qtah.Internal.Interface.QLabel
import Graphics.UI.Qtah.Internal.Interface.QLayout
import Graphics.UI.Qtah.Internal.Interface.QLayoutItem
import Graphics.UI.Qtah.Internal.Interface.QLineEdit
import Graphics.UI.Qtah.Internal.Interface.QMainWindow
import Graphics.UI.Qtah.Internal.Interface.QObject
import Graphics.UI.Qtah.Internal.Interface.QPushButton
import Graphics.UI.Qtah.Internal.Interface.QString
import Graphics.UI.Qtah.Internal.Interface.QTextEdit
import Graphics.UI.Qtah.Internal.Interface.QVBoxLayout
import Graphics.UI.Qtah.Internal.Interface.QWidget
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (
  dropTrailingPathSeparator,
  replaceBaseName,
  takeDirectory,
  takeBaseName,
  takeFileName,
  )

bindingImports :: [Include]
bindingImports =
  [ includeStd "cstring"
  , includeStd "QAbstractButton"
  , includeStd "QAbstractScrollArea"
  , includeStd "QApplication"
  , includeStd "QBoxLayout"
  , includeStd "QFrame"
  , includeStd "QHBoxLayout"
  , includeStd "QLabel"
  , includeStd "QLayout"
  , includeStd "QLayoutItem"
  , includeStd "QLineEdit"
  , includeStd "QMainWindow"
  , includeStd "QObject"
  , includeStd "QPushButton"
  , includeStd "QString"
  , includeStd "QTextEdit"
  , includeStd "QVBoxLayout"
  , includeStd "QWidget"
  , includeLocal "listeners.hpp"
  , includeLocal "shim_qapplication.hpp"
  ]

callbackImports :: [Include]
callbackImports =
  [ includeStd "cstring"
  , includeStd "string"
  , includeStd "QObject"
  ]

nonQtModuleExports :: [Export]
nonQtModuleExports =
  [ ExportClass c_std__string
  ] ++
  map ExportCallback allCallbacks ++
  map ExportClass allListeners

allQtModules :: [QtModule]
allQtModules =
  [ mod_QAbstractButton
  , mod_QAbstractScrollArea
  , mod_QApplication
  , mod_QBoxLayout
  , mod_QCoreApplication
  , mod_QFrame
  , mod_QHBoxLayout
  , mod_QLabel
  , mod_QLayout
  , mod_QLayoutItem
  , mod_QLineEdit
  , mod_QMainWindow
  , mod_QObject
  , mod_QPushButton
  , mod_QString
  , mod_QTextEdit
  , mod_QVBoxLayout
  , mod_QWidget
  ]

interfaceResult :: Either String Interface
interfaceResult =
  interface "qtah"
  "bindings.cpp" "bindings.hpp" bindingImports
  (Just ("callbacks.cpp", "callbacks.hpp", callbackImports))
  allExports
  where allExports =
          nonQtModuleExports ++
          concatMap (map qtExportToExport . qtModuleExports) allQtModules

main :: IO ()
main = do
  case interfaceResult of
    Left errorMsg -> do
      putStrLn $ "Error initializing interface: " ++ errorMsg
      exitFailure
    Right iface -> do
      args <- getArgs
      actions <- run [iface] args
      forM_ actions $ \action -> case action of
        GenHaskell path -> do
          -- Generate bindings for all Qt signals into one file.
          let baseModuleName = takeBaseName path
              signalModuleName = moduleNameToSignalModuleName baseModuleName
              signalPath = replaceBaseName path signalModuleName
              signalGeneration =
                execGenerator $ generateSignals baseModuleName $
                mapMaybe (\export -> case export of
                             QtExportFn {} -> Nothing
                             QtExportClass qtCls -> Just qtCls
                             QtExportCallback {} -> Nothing) $
                concatMap qtModuleExports allQtModules
          case signalGeneration of
            Left errorMsg -> do
              putStrLn $ "Error generating Qt signal bindings: " ++ errorMsg
              exitFailure
            Right body -> writeFile signalPath body

          -- Generate nicely-named Qt modules that will point to the bindings.
          srcDir <- maybeFail ("Couldn't find src directory for path " ++ show path ++
                               " to generate Qt modules.") $
                    findSrcDir path
          forM_ allQtModules $
            generateModule srcDir "Graphics.UI.Qtah" "Foreign.Cppop.Generated.Qtah"

        _ -> return ()

findSrcDir :: FilePath -> Maybe FilePath
findSrcDir = go . dropTrailingPathSeparator
  where go "" = Nothing
        go path =
          let dir = takeDirectory path
              file = takeFileName path
          in if file == "src" then Just path
             else if dir == path
                  then Nothing  -- Can't go up any more.
                  else go dir
