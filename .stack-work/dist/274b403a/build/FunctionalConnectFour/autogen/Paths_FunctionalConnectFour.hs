{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_FunctionalConnectFour (
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

bindir     = "C:\\Users\\garre\\OneDrive\\Documents\\Hendrix Coursework\\Major Courses\\Computer Science\\Functional Programming\\FP Final Project\\FunctionalConnectFour\\.stack-work\\install\\5c336a55\\bin"
libdir     = "C:\\Users\\garre\\OneDrive\\Documents\\Hendrix Coursework\\Major Courses\\Computer Science\\Functional Programming\\FP Final Project\\FunctionalConnectFour\\.stack-work\\install\\5c336a55\\lib\\x86_64-windows-ghc-8.10.7\\FunctionalConnectFour-0.1.0.0-8vss62co7Sj5odjNwwqiTv-FunctionalConnectFour"
dynlibdir  = "C:\\Users\\garre\\OneDrive\\Documents\\Hendrix Coursework\\Major Courses\\Computer Science\\Functional Programming\\FP Final Project\\FunctionalConnectFour\\.stack-work\\install\\5c336a55\\lib\\x86_64-windows-ghc-8.10.7"
datadir    = "C:\\Users\\garre\\OneDrive\\Documents\\Hendrix Coursework\\Major Courses\\Computer Science\\Functional Programming\\FP Final Project\\FunctionalConnectFour\\.stack-work\\install\\5c336a55\\share\\x86_64-windows-ghc-8.10.7\\FunctionalConnectFour-0.1.0.0"
libexecdir = "C:\\Users\\garre\\OneDrive\\Documents\\Hendrix Coursework\\Major Courses\\Computer Science\\Functional Programming\\FP Final Project\\FunctionalConnectFour\\.stack-work\\install\\5c336a55\\libexec\\x86_64-windows-ghc-8.10.7\\FunctionalConnectFour-0.1.0.0"
sysconfdir = "C:\\Users\\garre\\OneDrive\\Documents\\Hendrix Coursework\\Major Courses\\Computer Science\\Functional Programming\\FP Final Project\\FunctionalConnectFour\\.stack-work\\install\\5c336a55\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "FunctionalConnectFour_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "FunctionalConnectFour_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "FunctionalConnectFour_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "FunctionalConnectFour_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "FunctionalConnectFour_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "FunctionalConnectFour_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
