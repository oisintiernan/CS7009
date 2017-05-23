{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_common (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\oisin\\Desktop\\CS7009\\CS7009\\webpage\\.stack-work\\install\\f92693fc\\bin"
libdir     = "C:\\Users\\oisin\\Desktop\\CS7009\\CS7009\\webpage\\.stack-work\\install\\f92693fc\\lib\\x86_64-windows-ghc-8.0.2\\common-0.1.0.0-DfTAEsKJRSCaORMdpVE57"
dynlibdir  = "C:\\Users\\oisin\\Desktop\\CS7009\\CS7009\\webpage\\.stack-work\\install\\f92693fc\\lib\\x86_64-windows-ghc-8.0.2"
datadir    = "C:\\Users\\oisin\\Desktop\\CS7009\\CS7009\\webpage\\.stack-work\\install\\f92693fc\\share\\x86_64-windows-ghc-8.0.2\\common-0.1.0.0"
libexecdir = "C:\\Users\\oisin\\Desktop\\CS7009\\CS7009\\webpage\\.stack-work\\install\\f92693fc\\libexec"
sysconfdir = "C:\\Users\\oisin\\Desktop\\CS7009\\CS7009\\webpage\\.stack-work\\install\\f92693fc\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "common_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "common_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "common_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "common_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "common_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "common_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
