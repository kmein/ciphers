{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_ciphers (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
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
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/kmein/.cabal/bin"
libdir     = "/home/kmein/.cabal/lib/x86_64-linux-ghc-7.10.3/ciphers-0.1.0.0-IB38R3DFBzL8vN5Js7sPzX"
datadir    = "/home/kmein/.cabal/share/x86_64-linux-ghc-7.10.3/ciphers-0.1.0.0"
libexecdir = "/home/kmein/.cabal/libexec"
sysconfdir = "/home/kmein/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ciphers_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ciphers_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "ciphers_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ciphers_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ciphers_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
