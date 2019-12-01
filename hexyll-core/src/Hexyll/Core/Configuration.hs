--------------------------------------------------------------------------------
-- | Exports a datastructure for the top-level hakyll configuration
module Hexyll.Core.Configuration
    ( Configuration (..)
    , shouldIgnoreFile
    , defaultConfiguration
    ) where


--------------------------------------------------------------------------------
import           Data.Default     (Default (..))
import           Data.List        (isPrefixOf, isSuffixOf)
import           System.Directory (canonicalizePath)
import           System.Exit      (ExitCode)
import           System.FilePath  (isAbsolute, normalise, takeFileName)
import           System.IO.Error  (catchIOError)
import           System.Process   (system)


--------------------------------------------------------------------------------
data Configuration = Configuration
    { -- | Directory in which the output written
      destinationDirectory :: FilePath
    , -- | Directory where hakyll's internal store is kept
      storeDirectory       :: FilePath
    , -- | Directory in which some temporary files will be kept
      tmpDirectory         :: FilePath
    , -- | Directory where hakyll finds the files to compile. This is @.@ by
      -- default.
      providerDirectory    :: FilePath
    , -- | Function to determine ignored files
      --
      -- In 'defaultConfiguration', the following files are ignored:
      --
      -- * files starting with a @.@
      --
      -- * files starting with a @#@
      --
      -- * files ending with a @~@
      --
      -- * files ending with @.swp@
      --
      -- Note that the files in 'destinationDirectory' and 'storeDirectory' will
      -- also be ignored. Note that this is the configuration parameter, if you
      -- want to use the test, you should use 'shouldIgnoreFile'.
      --
      ignoreFile           :: FilePath -> Bool
    , -- | Use an in-memory cache for items. This is faster but uses more
      -- memory.
      inMemoryCache        :: Bool
    }

--------------------------------------------------------------------------------
instance Default Configuration where
    def = defaultConfiguration

--------------------------------------------------------------------------------
-- | Default configuration for a hakyll application
defaultConfiguration :: Configuration
defaultConfiguration = Configuration
    { destinationDirectory = "_site"
    , storeDirectory       = "_cache"
    , tmpDirectory         = "_cache/tmp"
    , providerDirectory    = "."
    , ignoreFile           = ignoreFile'
    , deployCommand        = "echo 'No deploy command specified' && exit 1"
    , deploySite           = system . deployCommand
    , inMemoryCache        = True
    , previewHost          = "127.0.0.1"
    , previewPort          = 8000
    }
  where
    ignoreFile' path
        | "."    `isPrefixOf` fileName = True
        | "#"    `isPrefixOf` fileName = True
        | "~"    `isSuffixOf` fileName = True
        | ".swp" `isSuffixOf` fileName = True
        | otherwise                    = False
      where
        fileName = takeFileName path


--------------------------------------------------------------------------------
-- | Check if a file should be ignored
shouldIgnoreFile :: Configuration -> FilePath -> IO Bool
shouldIgnoreFile conf path = orM
    [ inDir (destinationDirectory conf)
    , inDir (storeDirectory conf)
    , inDir (tmpDirectory conf)
    , return (ignoreFile conf path')
    ]
  where
    path'    = normalise path
    absolute = isAbsolute path

    inDir dir
        | absolute  = do
            dir' <- catchIOError (canonicalizePath dir) (const $ return dir)
            return $ dir' `isPrefixOf` path'
        | otherwise = return $ dir `isPrefixOf` path'

    orM :: [IO Bool] -> IO Bool
    orM []       = return False
    orM (x : xs) = x >>= \b -> if b then return True else orM xs
