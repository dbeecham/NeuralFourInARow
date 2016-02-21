module Paths_NeuralFourInARow (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/dbe/.cabal/bin"
libdir     = "/home/dbe/.cabal/lib/x86_64-linux-ghc-7.10.3/NeuralFourInARow-0.1.0.0-HzGVXwB1SIsCNNGlTFB4bw"
datadir    = "/home/dbe/.cabal/share/x86_64-linux-ghc-7.10.3/NeuralFourInARow-0.1.0.0"
libexecdir = "/home/dbe/.cabal/libexec"
sysconfdir = "/home/dbe/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "NeuralFourInARow_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "NeuralFourInARow_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "NeuralFourInARow_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "NeuralFourInARow_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "NeuralFourInARow_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
