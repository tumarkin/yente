module Paths_yente (
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
version = Version [0,2,1,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/roberttumarkin/Sandboxes/yente/bin"
libdir     = "/Users/roberttumarkin/Sandboxes/yente/lib/x86_64-osx-ghc-7.8.3/yente-0.2.1.0"
datadir    = "/Users/roberttumarkin/Sandboxes/yente/share/x86_64-osx-ghc-7.8.3/yente-0.2.1.0"
libexecdir = "/Users/roberttumarkin/Sandboxes/yente/libexec"
sysconfdir = "/Users/roberttumarkin/Sandboxes/yente/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "yente_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "yente_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "yente_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "yente_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "yente_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
