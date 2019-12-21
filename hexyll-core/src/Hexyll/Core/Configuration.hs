{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module:      Hexyll.Core.Configuration
-- Copyright:   (c) 2019 Hexirp
-- License:     Apache-2.0
-- Maintainer:  https://github.com/Hexirp/hexirp-hakyll
-- Stability:   stable
-- Portability: non-portable (GHC Extensions: CPP + TemplateHaskell)
--
-- This module defines a datastructure for the top-level hexyll configuration.
module Hexyll.Core.Configuration
  ( Configuration (..)
  , defaultConfiguration
  , defaultIgnoreFile
  , shouldIgnoreFile
  ) where

  import Prelude

  import Data.List    (isPrefixOf, isSuffixOf)
  import Data.Default (Default (..))

  import Path

  import System.Exit    (ExitCode)
  import System.Process (system)

  -- | The top-level hexyll configration.
  --
  -- 'providerDirectory' is the current directory @.@ by default. See
  -- 'defaultConfiguration' if you want more information about the default
  -- values.
  --
  -- Note that in addition to 'ignoreFile', the files in 'destinationDirectory',
  -- 'storeDirectory', and 'tmpDirectory' will also be ignored. If you want to
  -- test whether a file is ignored, you should use 'shouldIgnoreFile' instead
  -- of 'ignoreFile'.
  --
  -- By using 'deployCommand', you can plug in a system command to upload/deploy
  -- your site unless you change 'deploySite' from the default. You can execute
  -- this by using:
  --
  -- > ./site deploy
  --
  -- If 'inMemoryCache' is true, hexyll will be faster but uses more memory.
  --
  -- @since 0.1.0.0
  data Configuration = Configuration
    { -- | Directory in which the output written.
      destinationDirectory :: Path Rel Dir
    , -- | Directory where hexyll's internal store is kept.
      storeDirectory       :: Path Rel Dir
    , -- | Directory in which some temporary files will be kept.
      tmpDirectory         :: Path Rel Dir
    , -- | Directory where hexyll finds the files to compile.
      providerDirectory    :: Path Rel Dir
    , -- | Function to determine ignored files.
      ignoreFile           :: Path Rel File -> Bool
    , -- | System command to upload/deploy your site.
      deployCommand        :: String
    , -- | Function to deploy the site from Haskell.
      deploySite           :: Configuration -> IO ExitCode
    , -- | Flag to use an in-memory cache for items.
      inMemoryCache        :: Bool
    }

  -- | @since 0.1.0.0
  instance Default Configuration where
    def = defaultConfiguration

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
  -- | Default configuration for a hexyll application.
  --
  -- 'ignoreFile' is set with 'defaultIgnoreFile'.
  --
  -- The 'Configuration' object is passed as a parameter to 'deploySite', then
  -- 'deploySite' executes the shell command stored in 'deployCommand'. If you
  -- override it, 'deployCommand' will not be used implicitely.
  --
  -- Default values:
  --
  -- >>> destinationDirectory defaultConfiguration
  -- "_site\\"
  --
  -- >>> storeDirectory defaultConfiguration
  -- "_cache\\"
  --
  -- >>> tmpDirectory defaultConfiguration
  -- "_cache\\tmp\\"
  --
  -- >>> providerDirectory defaultConfiguration
  -- ".\\"
  --
  -- >>> deployCommand defaultConfiguration
  -- "echo 'No deploy command specified' && exit 1"
  --
  -- >>> inMemoryCache defaultConfiguration
  -- True
  --
  -- @since 0.1.0.0
#else
  -- | Default configuration for a hexyll application.
  --
  -- 'ignoreFile' is set with 'defaultIgnoreFile'.
  --
  -- The 'Configuration' object is passed as a parameter to 'deploySite', then
  -- 'deploySite' executes the shell command stored in 'deployCommand'. If you
  -- override it, 'deployCommand' will not be used implicitely.
  --
  -- Default values:
  --
  -- >>> destinationDirectory defaultConfiguration
  -- "_site/"
  --
  -- >>> storeDirectory defaultConfiguration
  -- "_cache/"
  --
  -- >>> tmpDirectory defaultConfiguration
  -- "_cache/tmp/"
  --
  -- >>> providerDirectory defaultConfiguration
  -- "./"
  --
  -- >>> deployCommand defaultConfiguration
  -- "echo 'No deploy command specified' && exit 1"
  --
  -- >>> inMemoryCache defaultConfiguration
  -- True
  --
  -- @since 0.1.0.0
#endif
  defaultConfiguration :: Configuration
  defaultConfiguration = Configuration
    { destinationDirectory = $(mkRelDir "_site")
    , storeDirectory       = $(mkRelDir "_cache")
    , tmpDirectory         = $(mkRelDir "_cache/tmp")
    , providerDirectory    = $(mkRelDir ".")
    , ignoreFile           = defaultIgnoreFile
    , deployCommand        = "echo 'No deploy command specified' && exit 1"
    , deploySite           = system . deployCommand
    , inMemoryCache        = True
    }

  -- | Default 'ignoreFile'.
  --
  -- In 'defaultIgnoreFile', the following files are ignored:
  --
  -- * Files starting with a @.@.
  -- * Files starting with a @#@.
  -- * Files ending with a @~@.
  -- * Files ending with @.swp@.
  --
  -- >>> defaultIgnoreFile <$> parseRelFile ".gitignore"
  -- True
  --
  -- >>> defaultIgnoreFile <$> parseRelFile "Configuration.hs.swp"
  -- True
  --
  -- >>> defaultIgnoreFile <$> parseRelFile "foo~"
  -- True
  --
  -- >>> defaultIgnoreFile <$> parseRelFile "#Main.hs#"
  -- True
  --
  -- >>> defaultIgnoreFile <$> parseRelFile "a.txt"
  -- False
  --
  -- >>> defaultIgnoreFile <$> parseRelFile ".dot/ma.x"
  -- False
  --
  -- >>> defaultIgnoreFile <$> parseRelFile "foo"
  -- False
  --
  -- @since 0.1.0.0
  defaultIgnoreFile :: Path Rel File -> Bool
  defaultIgnoreFile path
      | "."    `isPrefixOf` fileName = True
      | "#"    `isPrefixOf` fileName = True
      | "~"    `isSuffixOf` fileName = True
      | ".swp" `isSuffixOf` fileName = True
      | otherwise                    = False
    where
      fileName = toFilePath $ filename path

  -- | Check if a file should be ignored.
  --
  -- In addition to 'ignoreFile', the files in 'destinationDirectory',
  -- 'storeDirectory', and 'tmpDirectory' will also be ignored.
  -- 'shouldIgnoreFile' will consider the condition.
  --
  -- @since 0.1.0.0
  shouldIgnoreFile :: Configuration -> Path Rel File -> IO Bool
  shouldIgnoreFile conf path = return $ or
    [ destinationDirectory conf `isProperPrefixOf` path
    , storeDirectory conf `isProperPrefixOf` path
    , tmpDirectory conf `isProperPrefixOf` path
    , ignoreFile conf path
    ]
