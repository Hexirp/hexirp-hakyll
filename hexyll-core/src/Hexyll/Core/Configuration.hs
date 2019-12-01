-- |
-- Module:      Hexyll.Core.COnfiguration
-- Copyright:   (c) 2019 Hexirp
-- License:     Apache-2.0
-- Maintainer:  https://github.com/Hexirp/hexirp-hakyll
-- Stability:   stable
-- Portability: portable
--
-- This module defines a datastructure for the top-level hexyll configuration.
module Hexyll.Core.Configuration
    ( Configuration (..)
    , shouldIgnoreFile
    , defaultConfiguration
    ) where


import           Data.Default     (Default (..))
import           Data.List        (isPrefixOf, isSuffixOf)
import           System.Directory (canonicalizePath)
import           System.Exit      (ExitCode)
import           System.FilePath  (isAbsolute, normalise, takeFileName)
import           System.IO.Error  (catchIOError)
import           System.Process   (system)

-- | Top-level hexyll configration.
--
-- @provideDirectory@ is the current directory @.@ by default.
-- See 'defaultCofiguration' if you want more information about the default
-- values.
--
-- @since 0.1.0.0
data Configuration = Configuration
    { -- | Directory in which the output written.
      destinationDirectory :: FilePath
    , -- | Directory where hexyll's internal store is kept.
      storeDirectory       :: FilePath
    , -- | Directory in which some temporary files will be kept.
      tmpDirectory         :: FilePath
    , -- | Directory where hexyll finds the files to compile.
      providerDirectory    :: FilePath
    , -- | Function to determine ignored files.
      ignoreFile           :: FilePath -> Bool
    , -- | System command to upload/deploy your site.
      deployCommand        :: String
    , -- | Function to deploy the site from Haskell.
      deploySite           :: Configuration -> IO ExitCode
    , -- | Flag to use an in-memory cache for items.
      inMemoryCache        :: Bool
    }

instance Default Configuration where
    def = defaultConfiguration

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
