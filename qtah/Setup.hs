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

import Control.Applicative ((<|>))
import Control.Monad (unless, when)
import Data.Char (isDigit)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Distribution.InstalledPackageInfo (libraryDirs)
import Distribution.Package (PackageName (PackageName), pkgName, unPackageName)
import Distribution.PackageDescription (
  FlagName (FlagName),
  HookedBuildInfo,
  PackageDescription,
  emptyBuildInfo,
  extraLibDirs,
  package,
  )
import Distribution.Simple (defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.LocalBuildInfo (
  LocalBuildInfo,
  absoluteInstallDirs,
  buildDir,
  installedPkgs,
  libdir,
  localPkgDescr,
  withPrograms,
  )
import Distribution.Simple.PackageIndex (lookupPackageName)
import Distribution.Simple.Program (
  Program,
  getProgramOutput,
  lookupProgram,
  runDbProgram,
  simpleProgram,
  )
import Distribution.Simple.Setup (
  ConfigFlags,
  CopyDest (NoCopyDest),
  configConfigurationsFlags,
  configVerbosity,
  copyVerbosity,
  fromFlagOrDefault,
  installVerbosity,
  )
import Distribution.Simple.UserHooks (
  UserHooks (
    hookedPrograms,
    copyHook,
    instHook,
    postConf,
    preBuild,
    preCopy,
    preInst,
    preReg,
    preTest
    ),
  )
import Distribution.Simple.Utils (die, installOrdinaryFile, notice)
import Distribution.Verbosity (Verbosity, normal)
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv, setEnv)
import System.FilePath ((</>), takeDirectory)

packageName :: String
-- Careful, this line is modified by set-qt-version.sh.
packageName = "qtah"

cppPackageName :: String
-- Careful, this line is modified by set-qt-version.sh.
cppPackageName = "qtah-cpp"

main :: IO ()
main = defaultMainWithHooks qtahHooks

qtahHooks :: UserHooks
qtahHooks = simpleUserHooks
  { hookedPrograms = [generatorProgram]
  , postConf = \_ cf _ lbi -> do libDir <- lookupQtahCppLibDir lbi
                                 storeQtahCppLibDir libDir
                                 generateSources cf lbi libDir
  , preBuild = \_ _ -> addLibDir
  , preTest = \_ _ -> addLibDir
  , preCopy = \_ _ -> addLibDir  -- Not sure if necessary, but doesn't hurt.
  , copyHook = \pd lbi uh cf -> do let verbosity = fromFlagOrDefault normal $ copyVerbosity cf
                                   doInstall verbosity pd lbi
                                   copyHook simpleUserHooks pd lbi uh cf
  , preInst = \_ _ -> addLibDir  -- Not sure if necessary, but doesn't hurt.
  , instHook = \pd lbi uh if' -> do let verbosity = fromFlagOrDefault normal $ installVerbosity if'
                                    doInstall verbosity pd lbi
                                    instHook simpleUserHooks pd lbi uh if'
  , preReg = \_ _ -> addLibDir  -- Necessary.
  }

qtahCppLibDirFile :: FilePath
qtahCppLibDirFile = "dist/build/qtah-cpp-libdir"

lookupQtahCppLibDir :: LocalBuildInfo -> IO String
lookupQtahCppLibDir localBuildInfo = do
  -- Look for an installed qtah-cpp package.
  qtahCppPkg <- case lookupPackageName (installedPkgs localBuildInfo) $
                     PackageName cppPackageName of
    [(_, [qtahCppPkg])] -> return qtahCppPkg
    results ->
      die $ concat
      [packageName, ": Failed to find a unique ", cppPackageName, " installation.  Found ",
       show results, "."]

  -- Look up the libDir of the qtah-cpp we found.  The filter here is for NixOS,
  -- where libraryDirs includes the library directories of dependencies as well.
  case filter (\x -> cppPackageName `isInfixOf` x) $ libraryDirs qtahCppPkg of
    [libDir] -> return libDir
    libDirs -> die $ concat
               [packageName, ": Expected a single library directory for ",
                cppPackageName, ", got ", show libDirs, "."]

storeQtahCppLibDir :: FilePath -> IO ()
storeQtahCppLibDir libDir = do
  createDirectoryIfMissing True $ takeDirectory qtahCppLibDirFile
  writeFile qtahCppLibDirFile libDir

addLibDir :: IO HookedBuildInfo
addLibDir = do
  qtahCppLibDir <- readFile qtahCppLibDirFile
  return (Just emptyBuildInfo {extraLibDirs = [qtahCppLibDir]}, [])

generatorProgram :: Program
generatorProgram = simpleProgram "qtah-generator"

generateSources :: ConfigFlags -> LocalBuildInfo -> FilePath -> IO ()
generateSources configFlags localBuildInfo qtahCppLibDir = do
  let verbosity = fromFlagOrDefault normal $ configVerbosity configFlags
      programDb = withPrograms localBuildInfo

  -- Parse the Qt version to use from flags and the environment, and export it
  -- to the generator.
  (_, qtVersion) <- exportQtVersion configFlags localBuildInfo

  -- Ensure that we're using the same version of Qt that qtah-cpp is.
  let qtahCppQtVersionFile = qtahCppLibDir </> "qtah-qt-version"
  qtahCppQtVersion <-
    (\contents -> case lines contents of
       [line] -> return line
       _ -> die $ concat
            [packageName, ": Expected a single line in ", qtahCppQtVersionFile, ", got ",
             show contents, "."]) =<<
    readFile qtahCppQtVersionFile
  when (qtVersion /= qtahCppQtVersion) $
    die $ concat
    [packageName, ": Qt version mismatch between ", packageName, " (", qtVersion, ") and ",
     cppPackageName, " (", qtahCppQtVersion, ").  Please reconfigure one or the other."]

  -- Generate binding source code.
  let srcDir = "dist" </> "build" </> "autogen"
  createDirectoryIfMissing True srcDir
  runDbProgram verbosity generatorProgram programDb ["--gen-hs", srcDir]

doInstall :: Verbosity -> PackageDescription -> LocalBuildInfo -> IO ()
doInstall verbosity packageDesc localBuildInfo = do
  -- Record what version of Qt we are using.
  let libDir = libdir $ absoluteInstallDirs packageDesc localBuildInfo NoCopyDest
  createDirectoryIfMissing True libDir
  installOrdinaryFile verbosity
                      (buildDir localBuildInfo </> "qtah-qt-version")
                      (libDir </> "qtah-qt-version")

-- | This function should be called in a 'postConf' hook.  It determines the
-- requested Qt version based on package flags and the program environment.
--
-- The mutually exclusive package flags @qt4@ and @qt5@ specify a preference on
-- a major version of Qt.  Additionally, the environment variable @QTAH_QT@ can
-- either be @x@ or @x.y@ to specify a major or minor version of Qt,
-- respectively.  If both QTAH_QT and a flag is specified, they must agree on
-- the major version of Qt.  If using QTAH_QT, it only needs to be set for the
-- configure phase.  If neither flags nor QTAH_QT are set, then the system
-- default Qt version (as determined by @qmake@) will be used.  This may be
-- influenced by @qtchooser@.
--
-- If this package's name ends with @-qtX@, then Qt X (major version only) is
-- used unconditionally.  This overrides the above methods.
--
-- This returns the preferred major version of Qt, if there is a preference
-- (@Maybe Int@), along with the Qt version string returned from qtah-generator
-- (@String@).
--
-- !!! KEEP THIS FUNCTION IN SYNC WITH qtah-cpp/Setup.hs !!!
exportQtVersion :: ConfigFlags -> LocalBuildInfo -> IO (Maybe Int, String)
exportQtVersion configFlags localBuildInfo = do
  let verbosity = fromFlagOrDefault normal $ configVerbosity configFlags
      programDb = withPrograms localBuildInfo

  -- Determine what version of Qt to use.
  let myName = unPackageName $ pkgName $ package $ localPkgDescr localBuildInfo
  maybeQtMajor <- case reverse myName of
    -- If the package name ends in "-qtX", then build for Qt X (whatever the
    -- available minor version is).  Ignore QTAH_QT and package flags.
    n:'t':'q':'-':_ | isDigit n -> do
      setEnv "QTAH_QT" [n]
      notice verbosity $ concat [packageName, ": Requesting Qt ", [n], " because of package name."]
      return $ Just (read [n] :: Int)

    -- Otherwise, we'll inspect the environment and flags.
    _ -> do
      -- Inspect the 'qt4' and 'qt5' package flags.
      let flags = configConfigurationsFlags configFlags
          qt4Flag = fromMaybe False $ lookup (FlagName "qt4") flags
          qt5Flag = fromMaybe False $ lookup (FlagName "qt5") flags
          qtFlag = if qt4Flag then Just 4 else if qt5Flag then Just 5 else Nothing
      when (qt4Flag && qt5Flag) $
        die $ concat
        [packageName, ": The qt4 and qt5 flags are mutually exclusive.  Please select at most one."]

      -- Inspect the QTAH_QT environment variable.
      qtahQtStr <- lookupEnv "QTAH_QT"
      qtahQtMajor <- case qtahQtStr of
        Just s | not $ null s -> do
          let majorStr = takeWhile (/= '.') s
          unless (all isDigit majorStr) $
            die $ concat [packageName, ": Invalid QTAH_QT value ", show s,
                          ".  Expected a numeric version string."]
          return $ Just (read majorStr :: Int)
        _ -> return Nothing

      -- Determine which version of Qt to use, and put it in QTAH_QT for the
      -- generator to pick up.
      case (qtahQtMajor, qtFlag) of
        -- If both QTAH_QT and one of the qtX flags above is set, then they must agree.
        (Just m, Just n) -> do
          when (m /= n) $
            die $ concat
            [packageName, ": QTAH_QT=", show $ fromMaybe "" qtahQtStr, " and the qt",
             show n, " flag conflict."]
        -- Otherwise, if QTAH_QT is not already set but we have a flag preference,
        -- then use QTAH_QT to tell qtah-generator about the flag.
        (Nothing, Just n) -> setEnv "QTAH_QT" $ show n
        _ -> return ()

      -- Log a message showing which Qt we're requesting.
      case (qtahQtMajor, qtFlag) of
        (Just m, _) ->
          notice verbosity $
          concat [packageName, ": Requesting Qt ", show m, " because of QTAH_QT=",
                  show $ fromMaybe "" qtahQtStr, "."]
        (_, Just n) ->
          notice verbosity $
          concat [packageName, ": Requesting Qt ", show n, " because of the qt", show n, " flag."]
        _ -> notice verbosity $ concat [packageName, ": Requesting system default Qt."]

      return $ qtahQtMajor <|> qtFlag

  -- Log a message showing which Qt qtah-generator is actually using.
  generatorConfiguredProgram <-
    maybe (die $ packageName ++ ": Couldn't find qtah-generator.  Is it installed?") return $
    lookupProgram generatorProgram programDb
  qtVersionOutput <- getProgramOutput verbosity generatorConfiguredProgram ["--qt-version"]
  qtVersion <- case lines qtVersionOutput of
    [line] -> return line
    _ -> die $ concat
         [packageName, ": Couldn't understand qtah-generator --qt-version output: ",
          show qtVersionOutput]
  notice verbosity $
    concat [packageName, ": Using Qt ", qtVersion, "."]

  -- Record the selected Qt version in a file for later installation.
  let qtVersionFile = buildDir localBuildInfo </> "qtah-qt-version"
  createDirectoryIfMissing True $ takeDirectory qtVersionFile
  writeFile qtVersionFile $ unlines [qtVersion]

  return (maybeQtMajor, qtVersion)
