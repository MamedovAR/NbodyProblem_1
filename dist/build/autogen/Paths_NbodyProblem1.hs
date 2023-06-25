{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_NbodyProblem1 (
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

bindir     = "/home/mar/.cabal/bin"
libdir     = "/home/mar/.cabal/lib/x86_64-linux-ghc-8.6.5/NbodyProblem1-0.1.0.0-AMnVERhWEKSHNX6dLiTbXR"
dynlibdir  = "/home/mar/.cabal/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/mar/.cabal/share/x86_64-linux-ghc-8.6.5/NbodyProblem1-0.1.0.0"
libexecdir = "/home/mar/.cabal/libexec/x86_64-linux-ghc-8.6.5/NbodyProblem1-0.1.0.0"
sysconfdir = "/home/mar/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "NbodyProblem1_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "NbodyProblem1_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "NbodyProblem1_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "NbodyProblem1_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "NbodyProblem1_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "NbodyProblem1_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
