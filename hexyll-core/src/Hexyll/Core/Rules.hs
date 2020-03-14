--------------------------------------------------------------------------------
-- | This module provides a declarative DSL in which the user can specify the
-- different rules used to run the compilers.
--
-- The convention is to just list all items in the 'Rules' monad, routes and
-- compilation rules.
--
-- A typical usage example would be:
--
-- > main = hakyll $ do
-- >     match "posts/*" $ do
-- >         route   (setExtension "html")
-- >         compile someCompiler
-- >     match "css/*" $ do
-- >         route   idRoute
-- >         compile compressCssCompiler
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Hexyll.Core.Rules
    ( Rules
    , match
    , matchMetadata
    , create
    , version
    , compile
    , route

      -- * Advanced usage
    , preprocess
    , Dependency (..)
    , rulesExtraDependenciesCache

    , Pattern (..)
    ) where


--------------------------------------------------------------------------------
import           Control.Monad.Reader           (ask, local)
import           Control.Monad.State            (get, modify, put)
import           Control.Monad.Trans            (liftIO)
import           Control.Monad.Writer           (censor, tell)
import           Data.Maybe                     (fromMaybe)
import qualified Data.Set                       as S


--------------------------------------------------------------------------------
import           Data.Binary                    (Binary)
import           Data.Typeable                  (Typeable)


--------------------------------------------------------------------------------
import           Hexyll.Core.Compiler.Internal  hiding ( Pattern, match )
import           Hexyll.Core.Dependencies
import           Hexyll.Core.Identifier
import           Hexyll.Core.Identifier.Pattern hiding ( Pattern, match )
import           Hexyll.Core.Item
import           Hexyll.Core.Item.SomeItem
import           Hexyll.Core.Metadata           hiding ( Pattern )
import qualified Hexyll.Core.Metadata as Meta   ( Pattern (..) )
import           Hexyll.Core.Routes             hiding ( Pattern, match )
import qualified Hexyll.Core.Routes as Route    ( Pattern (..) )
import           Hexyll.Core.Rules.Internal
import           Hexyll.Core.Writable


--------------------------------------------------------------------------------
-- | Add a route
tellRoute :: Routes -> Rules ()
tellRoute route' = Rules $ tell $ RuleSet route' mempty mempty mempty


--------------------------------------------------------------------------------
-- | Add a number of compilers
tellCompilers :: [(Identifier, Compiler SomeItem)] -> Rules ()
tellCompilers compilers = Rules $ tell $ RuleSet mempty compilers mempty mempty


--------------------------------------------------------------------------------
-- | Add resources
tellResources :: [Identifier] -> Rules ()
tellResources resources' = Rules $ tell $
    RuleSet mempty mempty (S.fromList resources') mempty


--------------------------------------------------------------------------------
-- | Add a pattern
tellPattern :: Pattern -> Rules ()
tellPattern pattern = Rules $ tell $ RuleSet mempty mempty mempty pattern


--------------------------------------------------------------------------------
flush :: Rules ()
flush = Rules $ do
    mcompiler <- rulesCompiler <$> get
    case mcompiler of
        Nothing       -> return ()
        Just compiler -> do
            matches' <- rulesMatches                  <$> ask
            version' <- rulesVersion                  <$> ask
            route'   <- fromMaybe mempty . rulesRoute <$> get

            -- The version is possibly not set correctly at this point (yet)
            let ids = map (setIdentifierVersion version') matches'

            {-
            ids      <- case fromLiteral pattern of
                Just id' -> return [setIdentVersion version' id']
                Nothing  -> do
                    ids <- unRules $ getMatches pattern
                    unRules $ tellResources ids
                    return $ map (setIdentVersion version') ids
            -}

            -- Create a fast pattern for routing that matches exactly the
            -- compilers created in the block given to match
            let fastPattern = fromList ids

            -- Write out the compilers and routes
            unRules $ tellRoute $ matchRoute (Route.Pattern fastPattern) route'
            unRules $ tellCompilers $ [(id', compiler) | id' <- ids]

    put $ emptyRulesState


--------------------------------------------------------------------------------
matchInternal :: Pattern -> Rules [Identifier] -> Rules () -> Rules ()
matchInternal pattern getIDs rules = do
    tellPattern pattern
    flush
    ids <- getIDs
    tellResources ids
    Rules $ local (setMatches ids) $ unRules $ rules >> flush
  where
    setMatches ids env = env {rulesMatches = ids}

--------------------------------------------------------------------------------
match :: Pattern -> Rules () -> Rules ()
match (Pattern pattern) = matchInternal (Pattern pattern) $ getMatches (Meta.Pattern $ fromDisj pattern)


--------------------------------------------------------------------------------
matchMetadata :: Pattern -> (Metadata -> Bool) -> Rules () -> Rules ()
matchMetadata (Pattern pattern) metadataPred = matchInternal (Pattern pattern) $
    map fst . filter (metadataPred . snd) <$> getMetadataMatches (Meta.Pattern $ fromDisj pattern)


--------------------------------------------------------------------------------
create :: [Identifier] -> Rules () -> Rules ()
create ids rules = do
    flush
    -- TODO Maybe check if the resources exist and call tellResources on that
    Rules $ local setMatches $ unRules $ rules >> flush
  where
    setMatches env = env {rulesMatches = ids}


--------------------------------------------------------------------------------
version :: String -> Rules () -> Rules ()
version v rules = do
    flush
    Rules $ local setIdentVersion' $ unRules $ rules >> flush
  where
    setIdentVersion' env = env {rulesVersion = Just v}


--------------------------------------------------------------------------------
-- | Add a compilation rule to the rules.
--
-- This instructs all resources to be compiled using the given compiler.
compile :: (Binary a, Typeable a, Writable a) => Compiler (Item a) -> Rules ()
compile compiler = Rules $ modify $ \s ->
    s {rulesCompiler = Just (fmap SomeItem compiler)}


--------------------------------------------------------------------------------
-- | Add a route.
--
-- This adds a route for all items matching the current pattern.
route :: Routes -> Rules ()
route route' = Rules $ modify $ \s -> s {rulesRoute = Just route'}


--------------------------------------------------------------------------------
-- | Execute an 'IO' action immediately while the rules are being evaluated.
-- This should be avoided if possible, but occasionally comes in useful.
preprocess :: IO a -> Rules a
preprocess = Rules . liftIO


--------------------------------------------------------------------------------
-- | Advanced usage: add extra dependencies to compilers. Basically this is
-- needed when you're doing unsafe tricky stuff in the rules monad, but you
-- still want correct builds.
--
-- A useful utility for this purpose is 'makePatternDependency'.
rulesExtraDependenciesCache :: [Dependency] -> [Identifier] -> Rules a -> Rules a
rulesExtraDependenciesCache deps cache rules =
    -- Note that we add the dependencies seemingly twice here. However, this is
    -- done so that 'rulesExtraDependencies' works both if we have something
    -- like:
    --
    -- > match "*.css" $ rulesExtraDependencies [foo] $ ...
    --
    -- and something like:
    --
    -- > rulesExtraDependencies [foo] $ match "*.css" $ ...
    --
    -- (1) takes care of the latter and (2) of the former.
    Rules $ censor fixRuleSet $ do
        x <- unRules rules
        fixCompiler
        return x
  where
    -- (1) Adds the dependencies to the compilers we are yet to create
    fixCompiler = modify $ \s -> case rulesCompiler s of
        Nothing -> s
        Just c  -> s
            { rulesCompiler = Just $ compilerTellDependenciesCache deps cache >> c
            }

    -- (2) Adds the dependencies to the compilers that are already in the ruleset
    fixRuleSet ruleSet = ruleSet
        { rulesCompilers =
            [ (i, compilerTellDependenciesCache deps cache >> c)
            | (i, c) <- rulesCompilers ruleSet
            ]
        }
