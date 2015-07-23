module Paths_OrchestrateDB (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/adriandawid/Library/Haskell/bin"
libdir     = "/Users/adriandawid/Library/Haskell/ghc-7.8.3-x86_64/lib/OrchestrateDB-0.1.0.0"
datadir    = "/Users/adriandawid/Library/Haskell/share/ghc-7.8.3-x86_64/OrchestrateDB-0.1.0.0"
libexecdir = "/Users/adriandawid/Library/Haskell/libexec"
sysconfdir = "/Users/adriandawid/Library/Haskell/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "OrchestrateDB_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "OrchestrateDB_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "OrchestrateDB_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "OrchestrateDB_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "OrchestrateDB_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
