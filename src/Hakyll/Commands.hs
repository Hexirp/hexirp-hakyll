 --------------------------------------------------------------------------------
-- | Implementation of Hakyll commands: build, preview...
{-# LANGUAGE CPP #-}
module Hakyll.Commands
    ( Check(..)
    , build
    , check
    , clean
    , preview
    , rebuild
    , server
    , deploy
    , watch
    ) where


--------------------------------------------------------------------------------
import           System.Exit                (ExitCode)


--------------------------------------------------------------------------------
import           Hakyll.Check               (Check(..))
import qualified Hakyll.Check               as Check
import           Hakyll.Core.Configuration
import           Hakyll.Core.Logger         (Logger)
import qualified Hakyll.Core.Logger         as Logger
import           Hakyll.Core.Rules
import           Hakyll.Core.Rules.Internal
import           Hakyll.Core.Runtime
import           Hakyll.Core.Util.File


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
-- | Preview the site
preview :: Configuration -> Logger -> Rules a -> Int -> IO ()
preview _ _ _ _ = previewServerDisabled


--------------------------------------------------------------------------------
-- | Watch and recompile for changes

watch :: Configuration -> Logger -> String -> Int -> Bool -> Rules a -> IO ()
watch _ _ _ _ _ _ = watchServerDisabled

--------------------------------------------------------------------------------
-- | Rebuild the site
rebuild :: Configuration -> Logger -> Rules a -> IO ExitCode
rebuild conf logger rules =
    clean conf logger >> build conf logger rules

--------------------------------------------------------------------------------
-- | Start a server
server :: Configuration -> Logger -> String -> Int -> IO ()
server _ _ _ _ = previewServerDisabled


--------------------------------------------------------------------------------
-- | Upload the site
deploy :: Configuration -> IO ExitCode
deploy conf = deploySite conf conf


--------------------------------------------------------------------------------
-- | Print a warning message about the preview serving not being enabled
previewServerDisabled :: IO ()
previewServerDisabled =
    mapM_ putStrLn
        [ "PREVIEW SERVER"
        , ""
        , "The preview server is not enabled in the version of Hakyll. To"
        , "enable it, set the flag to True and recompile Hakyll."
        , "Alternatively, use an external tool to serve your site directory."
        ]

watchServerDisabled :: IO ()
watchServerDisabled =
    mapM_ putStrLn
      [ "WATCH SERVER"
      , ""
      , "The watch server is not enabled in the version of Hakyll. To"
      , "enable it, set the flag to True and recompile Hakyll."
      , "Alternatively, use an external tool to serve your site directory."
      ]
