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

import Control.Arrow (first)
import Control.Monad (forM_, when)
import Data.List (isPrefixOf, isSuffixOf)
import Distribution.PackageDescription (
  HookedBuildInfo,
  PackageDescription,
  emptyBuildInfo,
  extraLibDirs,
  )
import Distribution.Simple (defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.LocalBuildInfo (
  LocalBuildInfo,
  absoluteInstallDirs,
  buildDir,
  dynlibdir,
  libdir,
  withPrograms,
  )
import Distribution.Simple.Program (
  Program,
  getDbProgramOutput,
  knownPrograms,
  lookupProgram,
  programName,
  runDbProgram,
  simpleProgram,
  )
import Distribution.Simple.Setup (
  BuildFlags,
  CleanFlags,
  ConfigFlags,
  CopyDest (NoCopyDest),
  CopyFlags,
  cleanVerbosity,
  configPrograms,
  flagToMaybe,
  fromFlagOrDefault,
  )
import Distribution.Simple.UserHooks (
  Args,
  UserHooks (
    buildHook,
    hookedPrograms,
    cleanHook,
    copyHook,
    instHook,
    postConf,
    preBuild,
    preConf,
    preTest
    ),
  )
import Distribution.Simple.Utils (info)
import Distribution.Simple.Utils (copyFiles, installExecutableFile)
import Distribution.Verbosity (normal, verbose)
import System.Directory (
  createDirectoryIfMissing,
  doesDirectoryExist,
  doesFileExist,
  getCurrentDirectory,
  getDirectoryContents,
  setCurrentDirectory,
  removeDirectoryRecursive,
  removeFile,
  )
import System.IO.Error (
  catchIOError,
  isDoesNotExistError,
  )
import System.FilePath ((</>), joinPath, takeDirectory)
import System.Posix (
  createSymbolicLink,
  getSymbolicLinkStatus,
  )

main :: IO ()
main = defaultMainWithHooks qtahHooks

qtahHooks :: UserHooks
qtahHooks = simpleUserHooks
  { hookedPrograms = [generatorProgram, listenerGenProgram, makeProgram, qmakeProgram]
  , postConf = \_ _ _ lbi -> generateSources lbi
  , preBuild = \_ _ -> addLibDir
  , buildHook = \pd lbi uh bf -> do doBuild pd lbi uh bf
                                    buildHook simpleUserHooks pd lbi uh bf
  , preTest = \_ _ -> addLibDir
  , copyHook = \pd lbi uh cf -> do putStrLn "<<< Qtah copyHook"
                                   installLib pd lbi
                                   putStrLn ">>> Qtah copyHook"
                                   copyHook simpleUserHooks pd lbi uh cf
  , instHook = \pd lbi uh if' -> do putStrLn "<<< Qtah instHook"
                                    installLib pd lbi
                                    putStrLn ">>> Qtah instHook"
                                    instHook simpleUserHooks pd lbi uh if'
  , cleanHook = \pd z uh cf -> do doClean cf
                                  cleanHook simpleUserHooks pd z uh cf
  }

generatorProgram :: Program
generatorProgram = simpleProgram "qtah-generator"

listenerGenProgram :: Program
listenerGenProgram = simpleProgram "qtah-listener-gen"

makeProgram :: Program
makeProgram = simpleProgram "make"

qmakeProgram :: Program
qmakeProgram = simpleProgram "qmake"

-- | Adds the C++ build directory to extra-lib-dirs.
addLibDir :: IO HookedBuildInfo
addLibDir = do
  startDir <- getCurrentDirectory
  let cppSourceDir = startDir </> "cpp"
  return (Just emptyBuildInfo {extraLibDirs = [cppSourceDir]}, [])

generateSources :: LocalBuildInfo -> IO ()
generateSources localBuildInfo = do
  startDir <- getCurrentDirectory
  let cppSourceDir = startDir </> "cpp"
      programDb = withPrograms localBuildInfo

  -- TODO Make qtah-generator take the Qt version at runtime, and check here
  -- that the Qt version matches.

  runDbProgram normal generatorProgram programDb ["--gen-cpp", "cpp", "--gen-hs", "src"]
  runDbProgram normal listenerGenProgram programDb ["--gen-cpp-dir", "cpp"]

  setCurrentDirectory cppSourceDir
  runDbProgram normal qmakeProgram programDb ["qtah.pro"]

  setCurrentDirectory startDir

doBuild :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
doBuild _ localBuildInfo _ buildFlags = do
  putStrLn "Building the Qtah C++ library..."
  startDir <- getCurrentDirectory
  let cppSourceDir = startDir </> "cpp"
      programDb = withPrograms localBuildInfo

  setCurrentDirectory cppSourceDir
  runDbProgram normal makeProgram programDb ["-j4"]
  -- TODO Pull -j from the environment, this isn't working.
  -- Also, buildNumJobs isn't present with GHC 7.8.4.
  -- maybe [] (\j -> ['-':'j':show j]) $ flagToMaybe $ buildNumJobs buildFlags

  setCurrentDirectory startDir

installLib :: PackageDescription -> LocalBuildInfo -> IO ()
installLib packageDesc localBuildInfo = do
  startDir <- getCurrentDirectory
  let cppSourceDir = startDir </> "cpp"
      libDir = libdir $ absoluteInstallDirs packageDesc localBuildInfo NoCopyDest
  createDirectoryIfMissing True libDir
  forM_ ["libqtah.so", "libqtah.so.0", "libqtah.so.0.1", "libqtah.so.0.1.0"] $ \p -> do
    let path = libDir </> p
    shouldDelete <-
      catchIOError (do getSymbolicLinkStatus path
                       return True)
      (\e -> if isDoesNotExistError e then return False else ioError e)
    when shouldDelete $ removeFile path
  installExecutableFile verbose
                        (cppSourceDir </> "libqtah.so.0.1.0")
                        (libDir </> "libqtah.so.0.1.0")
  createSymbolicLink "libqtah.so.0.1.0" (libDir </> "libqtah.so.0.1")
  createSymbolicLink "libqtah.so.0.1" (libDir </> "libqtah.so.0")
  createSymbolicLink "libqtah.so.0" (libDir </> "libqtah.so")

doClean :: CleanFlags -> IO ()
doClean cleanFlags = do
  startDir <- getCurrentDirectory

  -- Remove cpp/{b_*,libqtah.so*,{callback,listener}.{cpp,hpp}}.
  let cppDir = startDir </> "cpp"
  cppDirExists <- doesDirectoryExist cppDir
  when cppDirExists $
    mapM_ (delFile False cppDir) .
    filter (\file ->
              "b_" `isPrefixOf` file ||
              "moc_" `isPrefixOf` file ||
              "libqtah.so" `isPrefixOf` file ||
              ".o" `isSuffixOf` file ||
              file `elem` ["Makefile",
                           "callback.cpp", "callback.hpp",
                           "listener.cpp", "listener.hpp"]) =<<
    getDirectoryContents cppDir

  -- Remove generated Haskell sources.
  delDir $ startDir </> joinPath ["src", "Graphics", "UI", "Qtah", "Generated"]
  delStartingWithInDir "Q" $ startDir </> joinPath ["src", "Graphics", "UI", "Qtah", "Core"]
  delStartingWithInDir "Q" $ startDir </> joinPath ["src", "Graphics", "UI", "Qtah", "Gui"]
  delStartingWithInDir "Q" $ startDir </> joinPath ["src", "Graphics", "UI", "Qtah", "Widgets"]
  delStartingWithInDir "Types.hs" $ startDir </> joinPath ["src", "Graphics", "UI", "Qtah", "Core"]
  delFile True (startDir </> joinPath ["src", "Graphics", "UI", "Qtah", "Internal"])
    "EventListener.hs"

  where verbosity = fromFlagOrDefault normal $ cleanVerbosity cleanFlags

        delFile checkExists dir file = do
          let path = dir </> file
          exists <- if checkExists
                    then doesFileExist path
                    else return True
          when exists $ do
            info verbosity $ concat ["Removing file ", path, "."]
            removeFile path

        delDir path = do
          exists <- doesDirectoryExist path
          when exists $ do
            info verbosity $ concat ["Removing directory ", path, "."]
            removeDirectoryRecursive path

        delIt dir file = do
          let path = dir </> file
          fileExists <- doesFileExist path
          if fileExists
            then delFile True dir file
            else do dirExists <- doesDirectoryExist path
                    when dirExists $ delDir path

        delStartingWithInDir prefix dir = do
          exists <- doesDirectoryExist dir
          when exists $
            mapM_ (delIt dir) .
            filter (prefix `isPrefixOf`) =<<
            getDirectoryContents dir
