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
version = Version {versionBranch = [1,0,0,2], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/adrian/.cabal/bin"
libdir     = "/home/adrian/.cabal/lib/x86_64-linux-ghc-7.6.3/OrchestrateDB-1.0.0.2"
datadir    = "/home/adrian/.cabal/share/x86_64-linux-ghc-7.6.3/OrchestrateDB-1.0.0.2"
libexecdir = "/home/adrian/.cabal/libexec"
sysconfdir = "/home/adrian/.cabal/etc"

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
