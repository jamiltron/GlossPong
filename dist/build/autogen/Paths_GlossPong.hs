module Paths_GlossPong (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/jamiltron/.cabal/bin"
libdir     = "/home/jamiltron/.cabal/lib/GlossPong-0.0.1/ghc-7.0.3"
datadir    = "/home/jamiltron/.cabal/share/GlossPong-0.0.1"
libexecdir = "/home/jamiltron/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "GlossPong_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "GlossPong_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "GlossPong_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "GlossPong_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
