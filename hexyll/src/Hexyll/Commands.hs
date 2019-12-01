 --------------------------------------------------------------------------------
-- | Implementation of Hexyll commands: build, preview...
{-# LANGUAGE CPP #-}
module Hexyll.Commands
    ( Check(..)
    , build
    , check
    , clean
    , rebuild
    , deploy
    ) where


--------------------------------------------------------------------------------
import           System.Exit                (ExitCode)


--------------------------------------------------------------------------------
import           Hexyll.Check               (Check(..))
import qualified Hexyll.Check               as Check
import           Hexyll.Core.Configuration
import           Hexyll.Core.Logger         (Logger)
import qualified Hexyll.Core.Logger         as Logger
import           Hexyll.Core.Rules
import           Hexyll.Core.Runtime
import           Hexyll.Core.Util.File


--------------------------------------------------------------------------------
-- | Build the site
build :: Configuration -> Logger -> Rules a -> IO ExitCode
build conf logger rules = fst <$> run conf logger rules


--------------------------------------------------------------------------------
-- | Run the checker and exit
check :: Configuration -> Logger -> Check.Check -> IO ExitCode
check = Check.check


--------------------------------------------------------------------------------
-- | Remove the output directories
clean :: Configuration -> Logger -> IO ()
clean conf logger = do
    remove $ destinationDirectory conf
    remove $ storeDirectory conf
    remove $ tmpDirectory conf
  where
    remove dir = do
        Logger.header logger $ "Removing " ++ dir ++ "..."
        removeDirectory dir


--------------------------------------------------------------------------------
-- | Rebuild the site
rebuild :: Configuration -> Logger -> Rules a -> IO ExitCode
rebuild conf logger rules =
    clean conf logger >> build conf logger rules

--------------------------------------------------------------------------------
-- | Upload the site
deploy :: Configuration -> IO ExitCode
deploy conf = deploySite conf conf
