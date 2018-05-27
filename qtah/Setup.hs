-- This file is part of Qtah.
--
-- Copyright 2015-2018 The Qtah Authors.
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
{-# LANGUAGE CPP, RankNTypes #-}

import Control.Applicative ((<|>))
import Control.Monad (unless, when)
import Data.Char (isDigit)
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (fromMaybe)
import Distribution.InstalledPackageInfo (libraryDirs)
import Distribution.Package (
  pkgName, unPackageName,
#if MIN_VERSION_Cabal(2,0,0)
  mkPackageName,
#else
  PackageName (PackageName),
#endif
  )
import Distribution.PackageDescription (
  HookedBuildInfo,
  PackageDescription,
  emptyBuildInfo,
  extraLibDirs,
  extraLibs,
  package,
#if MIN_VERSION_Cabal(2,0,0)
  mkFlagName,
#else
  FlagName (FlagName),
#endif
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
  CleanFlags,
  ConfigFlags,
  CopyDest (CopyTo, NoCopyDest),
  cleanVerbosity,
  configConfigurationsFlags,
  configVerbosity,
  copyDest,
  copyVerbosity,
  flagToMaybe,
  fromFlagOrDefault,
  installDistPref,
  installVerbosity,
  )
import Distribution.Simple.UserHooks (
  UserHooks (
    hookedPrograms,
    cleanHook,
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
#if MIN_VERSION_Cabal(2,0,0)
import Distribution.Simple.Utils (die')
#else
import Distribution.Simple.Utils (die)
#endif
import Distribution.Simple.Utils (info, installOrdinaryFile, notice)
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
import System.Environment (lookupEnv, setEnv)
import System.FilePath ((</>), joinPath, takeDirectory)

type DieFn = forall a. String -> IO a

#if !MIN_VERSION_Cabal(2,0,0)
mkPackageName = PackageName
mkFlagName = FlagName
#endif

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
  , postConf = \args cf pd lbi -> do libDir <-
                                       lookupQtahCppLibDir lbi
#if MIN_VERSION_Cabal(2,0,0)
                                       (die' $ fromFlagOrDefault normal $ configVerbosity cf)
#else
                                       die
#endif
                                     storeQtahCppLibDir libDir
                                     generateSources cf lbi libDir
                                     postConf simpleUserHooks args cf pd lbi
  , preBuild = \_ _ -> addLibDir
  , preTest = \_ _ -> addLibDir
  , preCopy = \_ _ -> addLibDir  -- Not sure if necessary, but doesn't hurt.
  , copyHook = \pd lbi uh cf -> do let verbosity = fromFlagOrDefault normal $ copyVerbosity cf
                                       dest = fromFlagOrDefault NoCopyDest $ copyDest cf
                                   doInstall verbosity pd lbi dest
                                   copyHook simpleUserHooks pd lbi uh cf
  , preInst = \_ _ -> addLibDir  -- Not sure if necessary, but doesn't hurt.
  , instHook = \pd lbi uh if' -> do let verbosity = fromFlagOrDefault normal $ installVerbosity if'
                                        dest = maybe NoCopyDest CopyTo $
                                               flagToMaybe $ installDistPref if'
                                    doInstall verbosity pd lbi dest
                                    instHook simpleUserHooks pd lbi uh if'
  , preReg = \_ _ -> addLibDir  -- Necessary.
  , cleanHook = \pd z uh cf -> do doClean cf
                                  cleanHook simpleUserHooks pd z uh cf
  }

qtahCppLibDirFile :: FilePath
qtahCppLibDirFile = "dist/build/qtah-cpp-libdir"

lookupQtahCppLibDir :: LocalBuildInfo -> DieFn -> IO String
lookupQtahCppLibDir localBuildInfo dieFn = do
  -- Look for an installed qtah-cpp package.
  qtahCppPkg <- case lookupPackageName (installedPkgs localBuildInfo) $
                     mkPackageName cppPackageName of
    [(_, [qtahCppPkg])] -> return qtahCppPkg
    results ->
      dieFn $ concat
      [packageName, ": Failed to find a unique ", cppPackageName, " installation.  Found ",
       show results, "."]

  -- Look up the libDir of the qtah-cpp we found.  The filter here is for NixOS,
  -- where libraryDirs includes the library directories of dependencies as well.
  case filter (\x -> cppPackageName `isInfixOf` x) $ libraryDirs qtahCppPkg of
    [libDir] -> return libDir
    libDirs -> dieFn $ concat
               [packageName, ": Expected a single library directory for ",
                cppPackageName, ", got ", show libDirs, "."]

storeQtahCppLibDir :: FilePath -> IO ()
storeQtahCppLibDir libDir = do
  createDirectoryIfMissing True $ takeDirectory qtahCppLibDirFile
  writeFile qtahCppLibDirFile libDir

addLibDir :: IO HookedBuildInfo
addLibDir = do
  qtahCppLibDir <- readFile qtahCppLibDirFile
  -- We add qtah to extra-libraries here, because we only know its path now,
  -- after the configure step.  If we put "extra-libraries: qtah" in qtah.cabal,
  -- then "cabal configure" fails because it can't find libqtah.so.
  return (Just emptyBuildInfo { extraLibDirs = [qtahCppLibDir]
                              , extraLibs = ["qtah"]
                              },
          [])

generatorProgram :: Program
generatorProgram = simpleProgram "qtah-generator"

generateSources :: ConfigFlags -> LocalBuildInfo -> FilePath -> IO ()
generateSources configFlags localBuildInfo qtahCppLibDir = do
  let verbosity = fromFlagOrDefault normal $ configVerbosity configFlags
      programDb = withPrograms localBuildInfo
#if MIN_VERSION_Cabal(2,0,0)
      dieFn = die' verbosity
#else
      dieFn = die
#endif

  -- Parse the Qt version to use from flags and the environment, and export it
  -- to the generator.
  qtVersion <- exportQtVersion configFlags localBuildInfo

  -- Ensure that we're using the same version of Qt that qtah-cpp is.
  let qtahCppQtVersionFile = qtahCppLibDir </> "qtah-qt-version"
  qtahCppQtVersion <-
    (\contents -> case lines contents of
       [line] -> return line
       _ -> dieFn $ concat
            [packageName, ": Expected a single line in ", qtahCppQtVersionFile, ", got ",
             show contents, "."]) =<<
    readFile qtahCppQtVersionFile
  when (qtVersion /= qtahCppQtVersion) $
    dieFn $ concat
    [packageName, ": Qt version mismatch between ", packageName, " (", qtVersion, ") and ",
     cppPackageName, " (", qtahCppQtVersion, ").  Please reconfigure one or the other."]

  -- Generate binding source code.
  runDbProgram verbosity generatorProgram programDb ["--gen-hs", "src"]

doInstall :: Verbosity -> PackageDescription -> LocalBuildInfo -> CopyDest -> IO ()
doInstall verbosity packageDesc localBuildInfo copyDest = do
  -- Record what version of Qt we are using.
  let libDir = libdir $ absoluteInstallDirs packageDesc localBuildInfo copyDest
  createDirectoryIfMissing True libDir
  installOrdinaryFile verbosity
                      (buildDir localBuildInfo </> "qtah-qt-version")
                      (libDir </> "qtah-qt-version")

doClean :: CleanFlags -> IO ()
doClean cleanFlags = do
  startDir <- getCurrentDirectory

  -- Remove generated Haskell sources.
  delDir $ startDir </> joinPath ["src", "Graphics", "UI", "Qtah", "Generated"]
  delStartingWithInDir "Q" $ startDir </> joinPath ["src", "Graphics", "UI", "Qtah", "Core"]
  delStartingWithInDir "Q" $ startDir </> joinPath ["src", "Graphics", "UI", "Qtah", "Gui"]
  delStartingWithInDir "Q" $ startDir </> joinPath ["src", "Graphics", "UI", "Qtah", "Widgets"]
  delStartingWithInDir "Types.hs" $ startDir </> joinPath ["src", "Graphics", "UI", "Qtah", "Core"]
  mapM_ (delFile True (startDir </> joinPath ["src", "Graphics", "UI", "Qtah", "Internal"]))
    ["Callback.hs", "EventListener.hs", "Listener.hs", "SceneEventListener.hs"]

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

-- | This function should be called in a 'postConf' hook.  It determines the
-- requested Qt version based on package flags and the program environment, and
-- sets the environment variables @QTAH_QT@ and @QT_SELECT@ appropriately.
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
exportQtVersion :: ConfigFlags -> LocalBuildInfo -> IO String
exportQtVersion configFlags localBuildInfo = do
  let verbosity = fromFlagOrDefault normal $ configVerbosity configFlags
      programDb = withPrograms localBuildInfo
#if MIN_VERSION_Cabal(2,0,0)
      dieFn = die' verbosity
#else
      dieFn = die
#endif

  -- Determine what version of Qt to use.  If we have a Qt version preference
  -- specified, either through package flags or through QTAH_QT, then
  -- maybeQtMajor will get that value.
  let myName = pkgName $ package $ localPkgDescr localBuildInfo
  maybeQtMajor <- case reverse $ unPackageName myName of
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
          qt4Flag = fromMaybe False $ lookup (mkFlagName "qt4") flags
          qt5Flag = fromMaybe False $ lookup (mkFlagName "qt5") flags
          qtFlag = if qt4Flag then Just 4 else if qt5Flag then Just 5 else Nothing
      when (qt4Flag && qt5Flag) $
        dieFn $ concat
        [packageName, ": The qt4 and qt5 flags are mutually exclusive.  Please select at most one."]

      -- Inspect the QTAH_QT environment variable.
      qtahQtStr <- lookupEnv "QTAH_QT"
      qtahQtMajor <- case qtahQtStr of
        Just s | not $ null s -> do
          let majorStr = takeWhile (/= '.') s
          unless (all isDigit majorStr) $
            dieFn $ concat [packageName, ": Invalid QTAH_QT value ", show s,
                          ".  Expected a numeric version string."]
          return $ Just (read majorStr :: Int)
        _ -> return Nothing

      -- Determine which version of Qt to use, and put it in QTAH_QT for the
      -- generator to pick up.
      case (qtahQtMajor, qtFlag) of
        -- If both QTAH_QT and one of the qtX flags above is set, then they must agree.
        (Just m, Just n) -> do
          when (m /= n) $
            dieFn $ concat
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

  -- If we have a major version preference, then set QT_SELECT in case we're
  -- calling QMake.  We use QT_SELECT over "-qt=X" because it doesn't break when
  -- qtchooser isn't available.
  case maybeQtMajor of
    Just qtMajor -> setEnv "QT_SELECT" $ show qtMajor
    Nothing -> return ()

  -- Log a message showing which Qt qtah-generator is actually using.
  generatorConfiguredProgram <-
    maybe (dieFn $ packageName ++ ": Couldn't find qtah-generator.  Is it installed?") return $
    lookupProgram generatorProgram programDb
  qtVersionOutput <- getProgramOutput verbosity generatorConfiguredProgram ["--qt-version"]
  qtVersion <- case lines qtVersionOutput of
    [line] -> return line
    _ -> dieFn $ concat
         [packageName, ": Couldn't understand qtah-generator --qt-version output: ",
          show qtVersionOutput]
  notice verbosity $
    concat [packageName, ": Using Qt ", qtVersion, "."]

  -- Record the selected Qt version in a file for later installation.
  let qtVersionFile = buildDir localBuildInfo </> "qtah-qt-version"
  createDirectoryIfMissing True $ takeDirectory qtVersionFile
  writeFile qtVersionFile $ unlines [qtVersion]

  return qtVersion
