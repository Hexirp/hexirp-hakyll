--------------------------------------------------------------------------------
-- | Module exporting convenient pandoc bindings
module Hexyll.Web.Pandoc
    ( -- * The basic building blocks
      readPandoc
    , readPandocWith
    , writePandoc
    , writePandocWith
    , renderPandoc
    , renderPandocWith

      -- * Derived compilers
    , pandocCompiler
    , pandocCompilerWith
    , pandocCompilerWithTransform
    , pandocCompilerWithTransformM

      -- * Default options
    , defaultHexyllReaderOptions
    , defaultHexyllWriterOptions
    ) where


--------------------------------------------------------------------------------
import qualified Data.Text                  as T
import           Text.Pandoc
import           Text.Pandoc.Highlighting   (pygments)


--------------------------------------------------------------------------------
import           Hexyll.Core.OldCompiler
import           Hexyll.Core.Item
import           Hexyll.Web.Pandoc.FileType


--------------------------------------------------------------------------------
-- | Read a string using pandoc, with the default options
readPandoc
    :: Item String             -- ^ String to read
    -> Compiler (Item Pandoc)  -- ^ Resulting document
readPandoc = readPandocWith defaultHexyllReaderOptions


--------------------------------------------------------------------------------
-- | Read a string using pandoc, with the supplied options
readPandocWith
    :: ReaderOptions           -- ^ Parser options
    -> Item String             -- ^ String to read
    -> Compiler (Item Pandoc)  -- ^ Resulting document
readPandocWith ropt item =
    case runPure $ traverse (reader ropt (itemFileType item)) (fmap T.pack item) of
        Left err    -> fail $
            "Hexyll.Web.Pandoc.readPandocWith: parse failed: " ++ show err
        Right item' -> return item'
  where
    reader ro t = case t of
        DocBook            -> readDocBook ro
        Html               -> readHtml ro
        LaTeX              -> readLaTeX ro
        LiterateHaskell t' -> reader (addExt ro Ext_literate_haskell) t'
        Markdown           -> readMarkdown ro
        MediaWiki          -> readMediaWiki ro
        OrgMode            -> readOrg ro
        Rst                -> readRST ro
        Textile            -> readTextile ro
        _                  -> error $
            "Hexyll.Web.readPandocWith: I don't know how to read a file of " ++
            "the type " ++ show t ++ " for: " ++ show (itemIdentifier item)

    addExt ro e = ro {readerExtensions = enableExtension e $ readerExtensions ro}


--------------------------------------------------------------------------------
-- | Write a document (as HTML) using pandoc, with the default options
writePandoc :: Item Pandoc  -- ^ Document to write
            -> Item String  -- ^ Resulting HTML
writePandoc = writePandocWith defaultHexyllWriterOptions


--------------------------------------------------------------------------------
-- | Write a document (as HTML) using pandoc, with the supplied options
writePandocWith :: WriterOptions  -- ^ Writer options for pandoc
                -> Item Pandoc    -- ^ Document to write
                -> Item String    -- ^ Resulting HTML
writePandocWith wopt (Item itemi doc) =
    case runPure $ writeHtml5String wopt doc of
        Left err    -> error $ "Hexyll.Web.Pandoc.writePandocWith: " ++ show err
        Right item' -> Item itemi $ T.unpack item'


--------------------------------------------------------------------------------
-- | Render the resource using pandoc
renderPandoc :: Item String -> Compiler (Item String)
renderPandoc =
    renderPandocWith defaultHexyllReaderOptions defaultHexyllWriterOptions


--------------------------------------------------------------------------------
-- | Render the resource using pandoc
renderPandocWith
    :: ReaderOptions -> WriterOptions -> Item String -> Compiler (Item String)
renderPandocWith ropt wopt item =
    writePandocWith wopt <$> readPandocWith ropt item


--------------------------------------------------------------------------------
-- | Read a page render using pandoc
pandocCompiler :: Compiler (Item String)
pandocCompiler =
    pandocCompilerWith defaultHexyllReaderOptions defaultHexyllWriterOptions


--------------------------------------------------------------------------------
-- | A version of 'pandocCompiler' which allows you to specify your own pandoc
-- options
pandocCompilerWith :: ReaderOptions -> WriterOptions -> Compiler (Item String)
pandocCompilerWith ropt wopt =
    cached "Hexyll.Web.Pandoc.pandocCompilerWith" $
        pandocCompilerWithTransform ropt wopt id


--------------------------------------------------------------------------------
-- | An extension of 'pandocCompilerWith' which allows you to specify a custom
-- pandoc transformation for the content
pandocCompilerWithTransform :: ReaderOptions -> WriterOptions
                            -> (Pandoc -> Pandoc)
                            -> Compiler (Item String)
pandocCompilerWithTransform ropt wopt f =
    pandocCompilerWithTransformM ropt wopt (return . f)


--------------------------------------------------------------------------------
-- | Similar to 'pandocCompilerWithTransform', but the transformation
-- function is monadic. This is useful when you want the pandoc
-- transformation to use the 'Compiler' information such as routes,
-- metadata, etc
pandocCompilerWithTransformM :: ReaderOptions -> WriterOptions
                    -> (Pandoc -> Compiler Pandoc)
                    -> Compiler (Item String)
pandocCompilerWithTransformM ropt wopt f =
    writePandocWith wopt <$>
        (traverse f =<< readPandocWith ropt =<< getResourceBody)


--------------------------------------------------------------------------------
-- | The default reader options for pandoc parsing in hakyll
defaultHexyllReaderOptions :: ReaderOptions
defaultHexyllReaderOptions = def
    { -- The following option causes pandoc to read smart typography, a nice
      -- and free bonus.
      readerExtensions = enableExtension Ext_smart pandocExtensions
    }


--------------------------------------------------------------------------------
-- | The default writer options for pandoc rendering in hakyll
defaultHexyllWriterOptions :: WriterOptions
defaultHexyllWriterOptions = def
    { -- This option causes literate haskell to be written using '>' marks in
      -- html, which I think is a good default.
      writerExtensions = enableExtension Ext_smart pandocExtensions
    , -- We want to have hightlighting by default, to be compatible with earlier
      -- Hexyll releases
      writerHighlightStyle = Just pygments
    }
