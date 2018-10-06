{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_conways_fight_for_survival (
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

bindir     = "/home/tano/IdeaProjects/conways-fight-for-survival/.stack-work/install/x86_64-linux-tinfo6/lts-12.11/8.4.3/bin"
libdir     = "/home/tano/IdeaProjects/conways-fight-for-survival/.stack-work/install/x86_64-linux-tinfo6/lts-12.11/8.4.3/lib/x86_64-linux-ghc-8.4.3/conways-fight-for-survival-0.1.0.0-3zO0m7zeBkU7PIvqhHIoK9-conways-fight-for-survival"
dynlibdir  = "/home/tano/IdeaProjects/conways-fight-for-survival/.stack-work/install/x86_64-linux-tinfo6/lts-12.11/8.4.3/lib/x86_64-linux-ghc-8.4.3"
datadir    = "/home/tano/IdeaProjects/conways-fight-for-survival/.stack-work/install/x86_64-linux-tinfo6/lts-12.11/8.4.3/share/x86_64-linux-ghc-8.4.3/conways-fight-for-survival-0.1.0.0"
libexecdir = "/home/tano/IdeaProjects/conways-fight-for-survival/.stack-work/install/x86_64-linux-tinfo6/lts-12.11/8.4.3/libexec/x86_64-linux-ghc-8.4.3/conways-fight-for-survival-0.1.0.0"
sysconfdir = "/home/tano/IdeaProjects/conways-fight-for-survival/.stack-work/install/x86_64-linux-tinfo6/lts-12.11/8.4.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "conways_fight_for_survival_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "conways_fight_for_survival_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "conways_fight_for_survival_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "conways_fight_for_survival_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "conways_fight_for_survival_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "conways_fight_for_survival_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
