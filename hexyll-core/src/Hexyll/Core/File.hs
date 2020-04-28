--------------------------------------------------------------------------------
-- | Exports simple compilers to just copy files
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hexyll.Core.File
    ( CopyFile (..)
    , copyFileCompiler
    , TmpFile (..)
    , newTmpFile
    ) where

import Prelude
import Path

--------------------------------------------------------------------------------
import           Data.Binary                   (Binary (..))
import           Data.Typeable                 (Typeable)
import           System.Directory              (copyFileWithMetadata)
import           System.Directory              (doesFileExist,
                                                renameFile)
import           System.Random                 (randomIO)


--------------------------------------------------------------------------------
import           Hexyll.Core.OldCompiler
import           Hexyll.Core.OldCompiler.Internal
import           Hexyll.Core.Configuration
import           Hexyll.Core.Item
import           Hexyll.Core.OldProvider
import qualified Hexyll.Core.OldStore          as Store
import           Hexyll.Core.Util.File
import           Hexyll.Core.OldWritable


--------------------------------------------------------------------------------
-- | This will copy any file directly by using a system call
newtype CopyFile = CopyFile FilePath
    deriving (Binary, Eq, Ord, Show, Typeable)


--------------------------------------------------------------------------------
instance Writable CopyFile where
    write dst (Item _ (CopyFile src)) = copyFileWithMetadata src dst


--------------------------------------------------------------------------------
copyFileCompiler :: Compiler (Item CopyFile)
copyFileCompiler = do
    identifier <- getUnderlying
    provider   <- compilerProvider <$> compilerAsk
    makeItem $ CopyFile $ resourceFilePath provider identifier


--------------------------------------------------------------------------------
newtype TmpFile = TmpFile FilePath
    deriving (Typeable)


--------------------------------------------------------------------------------
instance Binary TmpFile where
    put _ = return ()
    get   = error $
        "Hexyll.Core.File.TmpFile: You tried to load a TmpFile, however, " ++
        "this is not possible since these are deleted as soon as possible."


--------------------------------------------------------------------------------
instance Writable TmpFile where
    write dst (Item _ (TmpFile fp)) = renameFile fp dst


--------------------------------------------------------------------------------
-- | Create a tmp file
newTmpFile :: String            -- ^ Suffix and extension
           -> Compiler TmpFile  -- ^ Resulting tmp path
newTmpFile suffix = do
    path <- mkPath
    compilerUnsafeIO $ makeDirectories path
    debugCompiler $ "newTmpFile " ++ path
    return $ TmpFile path
  where
    mkPath = do
        rand <- compilerUnsafeIO $ randomIO :: Compiler Int
        tmp  <- toFilePath . tmpDirectory . compilerConfig <$> compilerAsk
        let path = tmp ++ Store.hash [show rand] ++ "-" ++ suffix
        exists <- compilerUnsafeIO $ doesFileExist path
        if exists then mkPath else return path
