-- This file is part of Qtah.
--
-- Copyright 2016 Bryan Gardiner <bog@khumba.net>
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

{-# OPTIONS_GHC -W -fwarn-incomplete-patterns -fwarn-unused-do-bind #-}
{-# LANGUAGE CPP #-}

import Control.Monad (forM_, join, when)
import Data.List (isPrefixOf, isSuffixOf)
import Distribution.PackageDescription (PackageDescription)
import Distribution.Simple (defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.LocalBuildInfo (
  LocalBuildInfo,
  absoluteInstallDirs,
  libdir,
  withPrograms,
  )
import Distribution.Simple.Program (
  Program,
  runDbProgram,
  simpleProgram,
  )
import Distribution.Simple.Setup (
  BuildFlags,
  CleanFlags,
  CopyDest (NoCopyDest),
  buildNumJobs,
  cleanVerbosity,
  copyVerbosity,
  flagToMaybe,
  fromFlagOrDefault,
  installVerbosity,
  )
import Distribution.Simple.UserHooks (
  UserHooks (
    buildHook,
    hookedPrograms,
    cleanHook,
    copyHook,
    instHook,
    postConf
    ),
  )
import Distribution.Simple.Utils (info)
import Distribution.Simple.Utils (installExecutableFile)
import Distribution.Verbosity (Verbosity, normal)
import System.Directory (
  createDirectoryIfMissing,
  doesDirectoryExist,
  getCurrentDirectory,
  getDirectoryContents,
  setCurrentDirectory,
  removeFile,
  )
import System.IO.Error (catchIOError, isDoesNotExistError)
import System.FilePath ((</>))
import System.Posix (createSymbolicLink, getSymbolicLinkStatus)

main :: IO ()
main = defaultMainWithHooks qtahHooks

qtahHooks :: UserHooks
qtahHooks = simpleUserHooks
  { hookedPrograms = [generatorProgram, listenerGenProgram, makeProgram, qmakeProgram]
  , postConf = \_ _ _ lbi -> generateSources lbi
  , buildHook = \pd lbi uh bf -> do doBuild lbi bf
                                    buildHook simpleUserHooks pd lbi uh bf
  , copyHook = \pd lbi uh cf -> do let verbosity = fromFlagOrDefault normal $ copyVerbosity cf
                                   installLib verbosity pd lbi
                                   copyHook simpleUserHooks pd lbi uh cf
  , instHook = \pd lbi uh if' -> do let verbosity = fromFlagOrDefault normal $ installVerbosity if'
                                    installLib verbosity pd lbi
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

generateSources :: LocalBuildInfo -> IO ()
generateSources localBuildInfo = do
  startDir <- getCurrentDirectory
  let cppSourceDir = startDir </> "cpp"
      programDb = withPrograms localBuildInfo

  -- TODO Make qtah-generator take the Qt version at runtime, and check here
  -- that the Qt version matches.

  runDbProgram normal generatorProgram programDb ["--gen-cpp", "cpp"]
  runDbProgram normal listenerGenProgram programDb ["--gen-cpp-dir", "cpp"]

  setCurrentDirectory cppSourceDir
  runDbProgram normal qmakeProgram programDb ["qtah.pro"]

  setCurrentDirectory startDir

doBuild :: LocalBuildInfo -> BuildFlags -> IO ()
doBuild localBuildInfo buildFlags = do
  startDir <- getCurrentDirectory
  let cppSourceDir = startDir </> "cpp"
      programDb = withPrograms localBuildInfo

  setCurrentDirectory cppSourceDir
  let (makeArgs, jobMsg) = case join $ flagToMaybe $ buildNumJobs buildFlags of
        Just n -> (["-j" ++ show n],
                   concat [" with ", show n, if n == 1 then " job" else " jobs"])
        Nothing -> ([], "")
  putStrLn $ concat ["Building the Qtah C++ library", jobMsg, "..."]
  runDbProgram normal makeProgram programDb makeArgs

  setCurrentDirectory startDir

installLib :: Verbosity -> PackageDescription -> LocalBuildInfo -> IO ()
installLib verbosity packageDesc localBuildInfo = do
  startDir <- getCurrentDirectory
  let cppSourceDir = startDir </> "cpp"
      libDir = libdir $ absoluteInstallDirs packageDesc localBuildInfo NoCopyDest
  createDirectoryIfMissing True libDir
  forM_ ["libqtah.so", "libqtah.so.0", "libqtah.so.0.1", "libqtah.so.0.1.0"] $ \p -> do
    let path = libDir </> p
    shouldDelete <-
      catchIOError (do _ <- getSymbolicLinkStatus path
                       return True)
      (\e -> if isDoesNotExistError e then return False else ioError e)
    when shouldDelete $ removeFile path
  installExecutableFile verbosity
                        (cppSourceDir </> "libqtah.so.0.1.0")
                        (libDir </> "libqtah.so.0.1.0")
  createSymbolicLink "libqtah.so.0.1.0" (libDir </> "libqtah.so.0.1")
  createSymbolicLink "libqtah.so.0.1" (libDir </> "libqtah.so.0")
  createSymbolicLink "libqtah.so.0" (libDir </> "libqtah.so")

doClean :: CleanFlags -> IO ()
doClean cleanFlags = do
  startDir <- getCurrentDirectory

  -- Remove generated C++ sources and build outputs.
  let cppDir = startDir </> "cpp"
  cppDirExists <- doesDirectoryExist cppDir
  when cppDirExists $
    mapM_ (delFile cppDir) .
    filter (\file ->
              "b_" `isPrefixOf` file ||
              "moc_" `isPrefixOf` file ||
              "libqtah.so" `isPrefixOf` file ||
              ".o" `isSuffixOf` file ||
              file `elem` ["Makefile",
                           "callback.cpp", "callback.hpp",
                           "listener.cpp", "listener.hpp"]) =<<
    getDirectoryContents cppDir

  where verbosity = fromFlagOrDefault normal $ cleanVerbosity cleanFlags

        delFile dir file = do
          let path = dir </> file
          info verbosity $ concat ["Removing file ", path, "."]
          removeFile path
