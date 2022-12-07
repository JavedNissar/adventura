{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_adventura (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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
version = Version [0,1,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "C:\\Users\\Xavier\\Documents\\Haskell\\adventura\\.stack-work\\install\\4863a622\\bin"
libdir     = "C:\\Users\\Xavier\\Documents\\Haskell\\adventura\\.stack-work\\install\\4863a622\\lib\\x86_64-windows-ghc-9.2.5\\adventura-0.1.0-L7vG1nHa5ywCfgnbajnF3j-adventura"
dynlibdir  = "C:\\Users\\Xavier\\Documents\\Haskell\\adventura\\.stack-work\\install\\4863a622\\lib\\x86_64-windows-ghc-9.2.5"
datadir    = "C:\\Users\\Xavier\\Documents\\Haskell\\adventura\\.stack-work\\install\\4863a622\\share\\x86_64-windows-ghc-9.2.5\\adventura-0.1.0"
libexecdir = "C:\\Users\\Xavier\\Documents\\Haskell\\adventura\\.stack-work\\install\\4863a622\\libexec\\x86_64-windows-ghc-9.2.5\\adventura-0.1.0"
sysconfdir = "C:\\Users\\Xavier\\Documents\\Haskell\\adventura\\.stack-work\\install\\4863a622\\etc"

getBinDir     = catchIO (getEnv "adventura_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "adventura_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "adventura_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "adventura_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "adventura_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "adventura_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
