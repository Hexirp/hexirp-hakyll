--------------------------------------------------------------------------------
-- | Wraps pandocs bibiliography handling
--
-- In order to add a bibliography, you will need a bibliography file (e.g.
-- @.bib@) and a CSL file (@.csl@). Both need to be compiled with their
-- respective compilers ('biblioCompiler' and 'cslCompiler'). Then, you can
-- refer to these files when you use 'readPandocBiblio'. This function also
-- takes the reader options for completeness -- you can use
-- 'defaultHexyllReaderOptions' if you're unsure.
-- 'pandocBiblioCompiler' is a convenience wrapper which works like 'pandocCompiler',
-- but also takes paths to compiled bibliography and csl files.
{-# LANGUAGE Arrows                     #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hexyll.Web.Pandoc.Biblio
    ( CSL
    , cslCompiler
    , Biblio (..)
    , biblioCompiler
    , readPandocBiblio
    , pandocBiblioCompiler
    ) where


--------------------------------------------------------------------------------
import           Control.Monad            (liftM, replicateM)
import           Data.Binary              (Binary (..))
import           Data.Typeable            (Typeable)
import           Hexyll.Core.OldCompiler
import           Hexyll.Core.OldCompiler.Internal
import           Hexyll.Core.Identifier
import           Hexyll.Core.Item
import           Hexyll.Core.OldProvider
import           Hexyll.Core.OldWritable
import           Hexyll.Web.Pandoc
import           Hexyll.Web.Pandoc.Binary ()
import qualified Text.CSL                 as CSL
import           Text.CSL.Pandoc          (processCites)
import           Text.Pandoc              (Pandoc, ReaderOptions (..),
                                           enableExtension, Extension (..))


--------------------------------------------------------------------------------
data CSL = CSL
    deriving (Show, Typeable)


--------------------------------------------------------------------------------
instance Binary CSL where
    put CSL = return ()
    get     = return CSL


--------------------------------------------------------------------------------
instance Writable CSL where
    -- Shouldn't be written.
    write _ _ = return ()


--------------------------------------------------------------------------------
cslCompiler :: Compiler (Item CSL)
cslCompiler = makeItem CSL


--------------------------------------------------------------------------------
newtype Biblio = Biblio [CSL.Reference]
    deriving (Show, Typeable)


--------------------------------------------------------------------------------
instance Binary Biblio where
    -- Ugly.
    get             = do
        len <- get
        Biblio <$> replicateM len get
    put (Biblio rs) = put (length rs) >> mapM_ put rs


--------------------------------------------------------------------------------
instance Writable Biblio where
    -- Shouldn't be written.
    write _ _ = return ()


--------------------------------------------------------------------------------
biblioCompiler :: Compiler (Item Biblio)
biblioCompiler = do
    filePath <- getResourceFilePath
    makeItem =<< unsafeCompiler (Biblio <$> CSL.readBiblioFile idpred filePath)
  where
    -- This is a filter on citations.  We include all citations.
    idpred = const True


--------------------------------------------------------------------------------
readPandocBiblio :: ReaderOptions
                 -> Item CSL
                 -> Item Biblio
                 -> (Item String)
                 -> Compiler (Item Pandoc)
readPandocBiblio ropt csl biblio item = do
    -- Parse CSL file, if given
    provider <- compilerProvider <$> compilerAsk
    style <- unsafeCompiler $
             CSL.readCSLFile Nothing . (resourceFilePath provider) . itemIdentifier $ csl

    -- We need to know the citation keys, add then *before* actually parsing the
    -- actual page. If we don't do this, pandoc won't even consider them
    -- citations!
    let Biblio refs = itemBody biblio
    pandoc <- itemBody <$> readPandocWith ropt item
    let pandoc' = processCites style refs pandoc

    return $ fmap (const pandoc') item

--------------------------------------------------------------------------------
pandocBiblioCompiler :: String -> String -> Compiler (Item String)
pandocBiblioCompiler cslFileName bibFileName = do
    csl <- load $ ufromFilePath cslFileName
    bib <- load $ ufromFilePath bibFileName
    liftM writePandoc
        (getResourceBody >>= readPandocBiblio ropt csl bib)
    where ropt = defaultHexyllReaderOptions
            { -- The following option enables citation rendering
              readerExtensions = enableExtension Ext_citations $ readerExtensions defaultHexyllReaderOptions
            }
