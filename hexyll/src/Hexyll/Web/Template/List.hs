--------------------------------------------------------------------------------
-- | Provides an easy way to combine several items in a list. The applications
-- are obvious:
--
-- * A post list on a blog
--
-- * An image list in a gallery
--
-- * A sitemap
{-# LANGUAGE TupleSections #-}
module Hexyll.Web.Template.List
    ( applyTemplateList
    , applyJoinTemplateList
    , chronological
    , recentFirst
    , sortChronological
    , sortRecentFirst
    ) where


--------------------------------------------------------------------------------
import           Control.Monad               (liftM)
import           Data.List                   (intersperse, sortBy)
import           Data.Ord                    (comparing)
import           Data.Time.Locale.Compat     (defaultTimeLocale)


--------------------------------------------------------------------------------
import           Hexyll.Core.Compiler
import           Hexyll.Core.Identifier
import           Hexyll.Core.Item
import           Hexyll.Core.Metadata
import           Hexyll.Web.Template
import           Hexyll.Web.Template.Context


--------------------------------------------------------------------------------
-- | Generate a string of a listing of pages, after applying a template to each
-- page.
applyTemplateList :: Template
                  -> Context a
                  -> [Item a]
                  -> Compiler String
applyTemplateList = applyJoinTemplateList ""


--------------------------------------------------------------------------------
-- | Join a listing of pages with a string in between, after applying a template
-- to each page.
applyJoinTemplateList :: String
                      -> Template
                      -> Context a
                      -> [Item a]
                      -> Compiler String
applyJoinTemplateList delimiter tpl context items = do
    items' <- mapM (applyTemplate tpl context) items
    return $ concat $ intersperse delimiter $ map itemBody items'


--------------------------------------------------------------------------------
-- | Sort pages chronologically. Uses the same method as 'dateField' for
-- extracting the date.
chronological :: MonadMetadata m => [Item a] -> m [Item a]
chronological =
    sortByM $ getItemUTC defaultTimeLocale . itemIdentifier
  where
    sortByM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [a]
    sortByM f xs = liftM (map fst . sortBy (comparing snd)) $
                   mapM (\x -> liftM (x,) (f x)) xs


--------------------------------------------------------------------------------
-- | The reverse of 'chronological'
recentFirst :: MonadMetadata m => [Item a] -> m [Item a]
recentFirst = liftM reverse . chronological


--------------------------------------------------------------------------------
-- | Version of 'chronological' which doesn't need the actual items.
sortChronological
    :: MonadMetadata m => [Identifier] -> m [Identifier]
sortChronological ids =
    liftM (map itemIdentifier) $ chronological [Item i () | i <- ids]


--------------------------------------------------------------------------------
-- | Version of 'recentFirst' which doesn't need the actual items.
sortRecentFirst
    :: MonadMetadata m => [Identifier] -> m [Identifier]
sortRecentFirst ids =
    liftM (map itemIdentifier) $ recentFirst [Item i () | i <- ids]
