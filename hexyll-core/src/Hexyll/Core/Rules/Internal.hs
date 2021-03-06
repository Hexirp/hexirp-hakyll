--------------------------------------------------------------------------------
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types                 #-}
module Hexyll.Core.Rules.Internal
    ( RulesRead (..)
    , RuleSet (..)
    , RulesState (..)
    , emptyRulesState
    , Rules (..)
    , runRules
    , Pattern (..)
    ) where


--------------------------------------------------------------------------------
import           Control.Monad.Reader           (ask)
import           Control.Monad.RWS              (RWST, runRWST)
import           Control.Monad.Trans            (liftIO)
import qualified Data.Map                       as M
import           Data.Set                       (Set)


--------------------------------------------------------------------------------
import           Hexyll.Core.OldCompiler.Internal  hiding ( Pattern )
import           Hexyll.Core.Identifier
import           Hexyll.Core.Identifier.Pattern hiding ( Pattern )
import           Hexyll.Core.Item.SomeItem
import           Hexyll.Core.Metadata           hiding ( Pattern )
import qualified Hexyll.Core.Metadata as Meta ( Pattern (..) )
import           Hexyll.Core.OldProvider
import           Hexyll.Core.OldRoutes             hiding ( Pattern )

import Data.Typeable ( Typeable )
import Data.String ( IsString (..) )


--------------------------------------------------------------------------------
data RulesRead = RulesRead
    { rulesProvider :: Provider
    , rulesMatches  :: [Identifier]
    , rulesVersion  :: Maybe String
    }


--------------------------------------------------------------------------------
data RuleSet = RuleSet
    { -- | Accumulated routes
      rulesRoutes    :: Routes
    , -- | Accumulated compilers
      rulesCompilers :: [(Identifier, Compiler SomeItem)]
    , -- | A set of the actually used files
      rulesResources :: Set Identifier
    , -- | A pattern we can use to check if a file *would* be used. This is
      -- needed for the preview server.
      rulesPattern   :: Pattern
    }


newtype Pattern = Pattern
  { unPattern :: PatternDisj
  } deriving ( Eq, Ord, Show, Typeable )

instance Semigroup Pattern where
  Pattern x <> Pattern y = Pattern (x <> y)

instance Monoid Pattern where
  mempty = Pattern mempty

instance IsString Pattern where
  fromString s = Pattern $ fromString s


--------------------------------------------------------------------------------
instance Semigroup RuleSet where
    RuleSet r1 c1 s1 p1 <> RuleSet r2 c2 s2 p2 = RuleSet (r1 <> r2) (c1 <> c2) (s1 <> s2) (p1 <> p2)

instance Monoid RuleSet where
    mempty  = RuleSet mempty mempty mempty mempty
    mappend = (<>)


--------------------------------------------------------------------------------
data RulesState = RulesState
    { rulesRoute    :: Maybe Routes
    , rulesCompiler :: Maybe (Compiler SomeItem)
    }


--------------------------------------------------------------------------------
emptyRulesState :: RulesState
emptyRulesState = RulesState Nothing Nothing


--------------------------------------------------------------------------------
-- | The monad used to compose rules
newtype Rules a = Rules
    { unRules :: RWST RulesRead RuleSet RulesState IO a
    } deriving (Monad, Functor, Applicative)


--------------------------------------------------------------------------------
instance MonadUniverse Rules where
    getMatches (Meta.Pattern pattern) = Rules $ do
        provider <- rulesProvider <$> ask
        return $ filter (`matchExpr` pattern) $ resourceList provider

instance MonadMetadata Rules where
    getMetadata identifier = Rules $ do
        provider <- rulesProvider <$> ask
        liftIO $ resourceMetadata provider identifier


--------------------------------------------------------------------------------
-- | Run a Rules monad, resulting in a 'RuleSet'
runRules :: Rules a -> Provider -> IO RuleSet
runRules rules provider = do
    (_, _, ruleSet) <- runRWST (unRules rules) env emptyRulesState

    -- Ensure compiler uniqueness
    let ruleSet' = ruleSet
            { rulesCompilers = M.toList $
                M.fromListWith (flip const) (rulesCompilers ruleSet)
            }

    return ruleSet'
  where
    env = RulesRead
        { rulesProvider = provider
        , rulesMatches  = []
        , rulesVersion  = Nothing
        }
