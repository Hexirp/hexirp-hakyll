--------------------------------------------------------------------------------
-- | Once a target is compiled, the user usually wants to save it to the disk.
-- This is where the 'Routes' type comes in; it determines where a certain
-- target should be written.
--
-- Suppose we have an item @foo\/bar.markdown@. We can render this to
-- @foo\/bar.html@ using:
--
-- > route "foo/bar.markdown" (setExtension ".html")
--
-- If we do not want to change the extension, we can use 'idRoute', the simplest
-- route available:
--
-- > route "foo/bar.markdown" idRoute
--
-- That will route @foo\/bar.markdown@ to @foo\/bar.markdown@.
--
-- Note that the extension says nothing about the content! If you set the
-- extension to @.html@, it is your own responsibility to ensure that the
-- content is indeed HTML.
--
-- Finally, some special cases:
--
-- * If there is no route for an item, this item will not be routed, so it will
--   not appear in your site directory.
--
-- * If an item matches multiple routes, the first rule will be chosen.
{-# LANGUAGE CPP        #-}
{-# LANGUAGE Rank2Types #-}
module Hexyll.Core.OldRoutes
    ( UsedMetadata
    , Routes
    , Pattern (..)
    , match
    , runRoutes
    , idRoute
    , setExtension
    , matchRoute
    , customRoute
    , constRoute
    , gsubRoute
    , metadataRoute
    , composeRoutes
    ) where


--------------------------------------------------------------------------------
import           System.FilePath                (replaceExtension)


--------------------------------------------------------------------------------
import           Hexyll.Core.Identifier
import           Hexyll.Core.Identifier.Pattern hiding ( Pattern, match )
import           Hexyll.Core.Metadata           hiding ( Pattern )
import           Hexyll.Core.OldProvider
import           Hexyll.Core.Util.String


import Data.Typeable ( Typeable )
import Data.String ( IsString (..) )

--------------------------------------------------------------------------------
-- | When you ran a route, it's useful to know whether or not this used
-- metadata. This allows us to do more granular dependency analysis.
type UsedMetadata = Bool


--------------------------------------------------------------------------------
data RoutesRead = RoutesRead
    { routesProvider   :: Provider
    , routesUnderlying :: Identifier
    }


--------------------------------------------------------------------------------
-- | Type used for a route
newtype Routes = Routes
    { unRoutes :: RoutesRead -> Identifier -> IO (Maybe FilePath, UsedMetadata)
    }


--------------------------------------------------------------------------------
instance Semigroup Routes where
    (<>) (Routes f) (Routes g) = Routes $ \p id' -> do
        (mfp, um) <- f p id'
        case mfp of
            Nothing -> g p id'
            Just _  -> return (mfp, um)

instance Monoid Routes where
    mempty  = Routes $ \_ _ -> return (Nothing, False)
    mappend = (<>)


newtype Pattern = Pattern { unPattern :: PatternExpr }
  deriving ( Eq, Ord, Show, Typeable )

instance IsString Pattern where
  fromString s = Pattern $ fromString s

match :: Identifier -> Pattern -> Bool
match i (Pattern p) = matchExpr i p

--------------------------------------------------------------------------------
-- | Apply a route to an identifier
runRoutes :: Routes -> Provider -> Identifier
          -> IO (Maybe FilePath, UsedMetadata)
runRoutes routes provider identifier =
    unRoutes routes (RoutesRead provider identifier) identifier


--------------------------------------------------------------------------------
-- | A route that uses the identifier as filepath. For example, the target with
-- ID @foo\/bar@ will be written to the file @foo\/bar@.
idRoute :: Routes
idRoute = customRoute fromIdentifierToFilePath


--------------------------------------------------------------------------------
-- | Set (or replace) the extension of a route.
--
-- Example:
--
-- > runRoutes (setExtension "html") "foo/bar"
--
-- Result:
--
-- > Just "foo/bar.html"
--
-- Example:
--
-- > runRoutes (setExtension "html") "posts/the-art-of-trolling.markdown"
--
-- Result:
--
-- > Just "posts/the-art-of-trolling.html"
setExtension :: String -> Routes
setExtension extension = customRoute $
    (`replaceExtension` extension) . fromIdentifierToFilePath


--------------------------------------------------------------------------------
-- | Apply the route if the identifier matches the given pattern, fail
-- otherwise
matchRoute :: Pattern -> Routes -> Routes
matchRoute pattern (Routes route) = Routes $ \p id' ->
    if match id' pattern then route p id' else return (Nothing, False)


--------------------------------------------------------------------------------
-- | Create a custom route. This should almost always be used with
-- 'matchRoute'
customRoute :: (Identifier -> FilePath) -> Routes
customRoute f = Routes $ const $ \id' -> return (Just (f id'), False)


--------------------------------------------------------------------------------
-- | A route that always gives the same result. Obviously, you should only use
-- this for a single compilation rule.
constRoute :: FilePath -> Routes
constRoute = customRoute . const


--------------------------------------------------------------------------------
-- | Create a gsub route
--
-- Example:
--
-- > runRoutes (gsubRoute "rss/" (const "")) "tags/rss/bar.xml"
--
-- Result:
--
-- > Just "tags/bar.xml"
gsubRoute :: String              -- ^ Pattern
          -> (String -> String)  -- ^ Replacement
          -> Routes              -- ^ Resulting route
gsubRoute pattern replacement = customRoute $
    replaceAll pattern replacement . fromIdentifierToFilePath


--------------------------------------------------------------------------------
-- | Get access to the metadata in order to determine the route
metadataRoute :: (Metadata -> Routes) -> Routes
metadataRoute f = Routes $ \r i -> do
    metadata <- resourceMetadata (routesProvider r) (routesUnderlying r)
    unRoutes (f metadata) r i


--------------------------------------------------------------------------------
-- | Compose routes so that @f \`composeRoutes\` g@ is more or less equivalent
-- with @g . f@.
--
-- Example:
--
-- > let routes = gsubRoute "rss/" (const "") `composeRoutes` setExtension "xml"
-- > in runRoutes routes "tags/rss/bar"
--
-- Result:
--
-- > Just "tags/bar.xml"
--
-- If the first route given fails, Hexyll will not apply the second route.
composeRoutes :: Routes  -- ^ First route to apply
              -> Routes  -- ^ Second route to apply
              -> Routes  -- ^ Resulting route
composeRoutes (Routes f) (Routes g) = Routes $ \p i -> do
    (mfp, um) <- f p i
    case mfp of
        Nothing -> return (Nothing, um)
        Just fp -> do
            (mfp', um') <- g p (ufromFilePath fp)
            return (mfp', um || um')
