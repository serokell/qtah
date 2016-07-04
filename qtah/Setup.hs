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

{-# OPTIONS_GHC -W -fwarn-incomplete-patterns -fwarn-unused-do-bind #-}
{-# LANGUAGE CPP #-}

import Control.Monad (when)
import Data.List (isInfixOf, isPrefixOf)
import Distribution.InstalledPackageInfo (libraryDirs)
import Distribution.Package (PackageName (PackageName))
import Distribution.PackageDescription (
  HookedBuildInfo,
  emptyBuildInfo,
  extraLibDirs,
  )
import Distribution.Simple (defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.LocalBuildInfo (
  LocalBuildInfo,
  installedPkgs,
  withPrograms,
  )
import Distribution.Simple.PackageIndex (lookupPackageName)
import Distribution.Simple.Program (
  Program,
  runDbProgram,
  simpleProgram,
  )
import Distribution.Simple.Setup (
  CleanFlags,
  cleanVerbosity,
  configVerbosity,
  fromFlagOrDefault,
  )
import Distribution.Simple.UserHooks (
  UserHooks (
    hookedPrograms,
    cleanHook,
    postConf,
    preBuild,
    preTest
    ),
  )
import Distribution.Simple.Utils (info)
import Distribution.Verbosity (Verbosity, normal)
import System.Directory (
  createDirectoryIfMissing,
  doesDirectoryExist,
  doesFileExist,
  getCurrentDirectory,
  getDirectoryContents,
  removeDirectoryRecursive,
  removeFile,
  )
import System.FilePath ((</>), joinPath, takeDirectory)

main :: IO ()
main = defaultMainWithHooks qtahHooks

qtahHooks :: UserHooks
qtahHooks = simpleUserHooks
  { hookedPrograms = [generatorProgram]
  , postConf = \_ cf _ lbi -> do storeQtahCppLibDir lbi
                                 generateSources (fromFlagOrDefault normal $ configVerbosity cf)
                                                 lbi
  , preBuild = \_ _ -> addLibDir
  , preTest = \_ _ -> addLibDir
  , cleanHook = \pd z uh cf -> do doClean cf
                                  cleanHook simpleUserHooks pd z uh cf
  }

qtahCppLibDirFile :: FilePath
qtahCppLibDirFile = "dist/build/qtah-cpp-libdir"

storeQtahCppLibDir :: LocalBuildInfo -> IO ()
storeQtahCppLibDir localBuildInfo = do
  let pkgs = installedPkgs localBuildInfo
      [(_, [qtahCppPkg])] = lookupPackageName pkgs $ PackageName "qtah-cpp"
  -- The filter here is for NixOS, where libraryDirs includes the library
  -- directories of dependencies as well.
  case filter (\x -> "qtah-cpp" `isInfixOf` x) $ libraryDirs qtahCppPkg of
    [libDir] -> do createDirectoryIfMissing True $ takeDirectory qtahCppLibDirFile
                   writeFile qtahCppLibDirFile libDir
    libDirs -> fail $ concat ["Expected a single library directory for qtah-cpp, got ",
                              show libDirs, "."]

addLibDir :: IO HookedBuildInfo
addLibDir = do
  qtahCppLibDir <- readFile "dist/build/qtah-cpp-libdir"
  return (Just emptyBuildInfo {extraLibDirs = [qtahCppLibDir]}, [])

generatorProgram :: Program
generatorProgram = simpleProgram "qtah-generator"

generateSources :: Verbosity -> LocalBuildInfo -> IO ()
generateSources verbosity localBuildInfo = do
  -- TODO Make qtah-generator take the Qt version at runtime, and check here
  -- that the Qt version matches.
  let programDb = withPrograms localBuildInfo
  runDbProgram verbosity generatorProgram programDb ["--gen-hs", "src"]

doClean :: CleanFlags -> IO ()
doClean cleanFlags = do
  startDir <- getCurrentDirectory

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
