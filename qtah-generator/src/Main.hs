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
import Graphics.UI.Qtah.Internal.Interface.QAbstractButton (mod_QAbstractButton)
import Graphics.UI.Qtah.Internal.Interface.QAbstractScrollArea (mod_QAbstractScrollArea)
import Graphics.UI.Qtah.Internal.Interface.QApplication (mod_QApplication)
import Graphics.UI.Qtah.Internal.Interface.QBoxLayout (mod_QBoxLayout)
import Graphics.UI.Qtah.Internal.Interface.QCoreApplication (mod_QCoreApplication)
import Graphics.UI.Qtah.Internal.Interface.QFrame (mod_QFrame)
import Graphics.UI.Qtah.Internal.Interface.QHBoxLayout (mod_QHBoxLayout)
import Graphics.UI.Qtah.Internal.Interface.QLabel (mod_QLabel)
import Graphics.UI.Qtah.Internal.Interface.QLayout (mod_QLayout)
import Graphics.UI.Qtah.Internal.Interface.QLayoutItem (mod_QLayoutItem)
import Graphics.UI.Qtah.Internal.Interface.QLineEdit (mod_QLineEdit)
import Graphics.UI.Qtah.Internal.Interface.QMainWindow (mod_QMainWindow)
import Graphics.UI.Qtah.Internal.Interface.QMargins (mod_QMargins)
import Graphics.UI.Qtah.Internal.Interface.QObject (mod_QObject)
import Graphics.UI.Qtah.Internal.Interface.QPoint (mod_QPoint)
import Graphics.UI.Qtah.Internal.Interface.QPushButton (mod_QPushButton)
import Graphics.UI.Qtah.Internal.Interface.QRect (mod_QRect)
import Graphics.UI.Qtah.Internal.Interface.QSize (mod_QSize)
import Graphics.UI.Qtah.Internal.Interface.QString (mod_QString)
import Graphics.UI.Qtah.Internal.Interface.Qt (mod_Qt)
import Graphics.UI.Qtah.Internal.Interface.QTextEdit (mod_QTextEdit)
import Graphics.UI.Qtah.Internal.Interface.QVBoxLayout (mod_QVBoxLayout)
import Graphics.UI.Qtah.Internal.Interface.QWidget (mod_QWidget)
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
  , includeStd "QPoint"
  , includeStd "QPushButton"
  , includeStd "QRect"
  , includeStd "QSize"
  , includeStd "QString"
  , includeStd "QTextEdit"
  , includeStd "QVBoxLayout"
  , includeStd "QWidget"
  , includeLocal "listeners.hpp"
  , includeLocal "encode.cpp"
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
  , mod_QMargins
  , mod_QObject
  , mod_QPoint
  , mod_QPushButton
  , mod_QRect
  , mod_QSize
  , mod_QString
  , mod_Qt
  , mod_QTextEdit
  , mod_QVBoxLayout
  , mod_QWidget
  ]

interfaceResult :: Either String Interface
interfaceResult =
  interface "qtah"
  "bindings.cpp" "bindings.hpp" bindingImports
  (Just ("callbacks.cpp", "callbacks.hpp", callbackImports))
  [ "qualified Graphics.UI.Qtah.H.HMargins as HMargins"
  , "qualified Graphics.UI.Qtah.H.HPoint as HPoint"
  , "qualified Graphics.UI.Qtah.H.HRect as HRect"
  , "qualified Graphics.UI.Qtah.H.HSize as HSize"
  ]
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
                             QtExportEnum {} -> Nothing
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
            generateModule srcDir "Graphics.UI.Qtah.Q" "Foreign.Cppop.Generated.Qtah"

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
