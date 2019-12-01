--------------------------------------------------------------------------------
-- | Module providing the main hakyll function and command-line argument parsing
{-# LANGUAGE CPP #-}

module Hexyll.Main
    ( hakyll
    , hakyllWith
    , hakyllWithArgs
    , hakyllWithExitCode
    , hakyllWithExitCodeAndArgs
    , Options(..)
    , Command(..)
    ) where


--------------------------------------------------------------------------------
import           System.Environment        (getProgName)
import           System.Exit               (ExitCode (ExitSuccess), exitWith)
import           System.IO.Unsafe          (unsafePerformIO)


--------------------------------------------------------------------------------
import           Data.Monoid               ((<>))
import qualified Options.Applicative       as OA


--------------------------------------------------------------------------------
import qualified Hexyll.Check              as Check
import qualified Hexyll.Commands           as Commands
import qualified Hexyll.Core.Configuration as Config
import qualified Hexyll.Core.Logger        as Logger
import           Hexyll.Core.Rules


--------------------------------------------------------------------------------
-- | This usually is the function with which the user runs the hakyll compiler
hakyll :: Rules a -> IO ()
hakyll = hakyllWith Config.defaultConfiguration

--------------------------------------------------------------------------------
-- | A variant of 'hakyll' which allows the user to specify a custom
-- configuration
hakyllWith :: Config.Configuration -> Rules a -> IO ()
hakyllWith conf rules = hakyllWithExitCode conf rules >>= exitWith

--------------------------------------------------------------------------------
-- | A variant of 'hakyll' which returns an 'ExitCode'
hakyllWithExitCode :: Config.Configuration -> Rules a -> IO ExitCode
hakyllWithExitCode conf rules =  do
    args <- defaultParser conf
    hakyllWithExitCodeAndArgs conf args rules

--------------------------------------------------------------------------------
-- | A variant of 'hakyll' which expects a 'Configuration' and command-line
-- 'Options'. This gives freedom to implement your own parsing.
hakyllWithArgs :: Config.Configuration -> Options -> Rules a -> IO ()
hakyllWithArgs conf args rules =
    hakyllWithExitCodeAndArgs conf args rules >>= exitWith

--------------------------------------------------------------------------------
hakyllWithExitCodeAndArgs :: Config.Configuration ->
                              Options -> Rules a -> IO ExitCode
hakyllWithExitCodeAndArgs conf args rules = do
    let args' = optCommand args
        verbosity' = if verbosity args then Logger.Debug else Logger.Message
        check     =
            if internal_links args' then Check.InternalLinks else Check.All

    logger <- Logger.new verbosity'
    invokeCommands args' conf check logger rules

--------------------------------------------------------------------------------
defaultParser :: Config.Configuration -> IO Options
defaultParser conf =
    OA.customExecParser (OA.prefs OA.showHelpOnError)
        (OA.info (OA.helper <*> optionParser conf)
        (OA.fullDesc <> OA.progDesc
        (progName ++ " - Static site compiler created with Hexyll")))


--------------------------------------------------------------------------------
invokeCommands :: Command -> Config.Configuration ->
                  Check.Check -> Logger.Logger -> Rules a -> IO ExitCode
invokeCommands args conf check logger rules =
    case args of
        Build          -> Commands.build conf logger rules
        Check   _      -> Commands.check conf logger check
        Clean          -> Commands.clean conf logger >> ok
        Deploy         -> Commands.deploy conf
        Rebuild        -> Commands.rebuild conf logger rules
    where
        ok = return ExitSuccess


--------------------------------------------------------------------------------

-- | The parsed command-line options.
data Options = Options {verbosity :: Bool, optCommand :: Command}
    deriving (Show)

-- | The command to run.
data Command
    = Build
    -- ^ Generate the site.
    | Check   {internal_links :: Bool}
    -- ^ Validate the site output.
    | Clean
    -- ^ Clean up and remove cache.
    | Deploy
    -- ^ Upload/deploy your site.
    | Rebuild
    -- ^ Clean and build again.
    deriving (Show)

optionParser :: Config.Configuration -> OA.Parser Options
optionParser conf = Options <$> verboseParser <*> commandParser conf
    where
    verboseParser = OA.switch (OA.long "verbose" <> OA.short 'v' <> OA.help "Run in verbose mode")


commandParser :: Config.Configuration -> OA.Parser Command
commandParser conf = OA.subparser $ foldr ((<>) . produceCommand) mempty commands
    where

    produceCommand (c,a,b) = OA.command c (OA.info (OA.helper <*> a) (b))

    commands =
        [ ( "build"
          , pure Build
          , OA.fullDesc <> OA.progDesc "Generate the site"
          )
        , ( "check"
          , pure Check <*> OA.switch (OA.long "internal-links" <> OA.help "Check internal links only")
          , OA.fullDesc <> OA.progDesc "Validate the site output"
          )
        , ( "clean"
          , pure Clean
          , OA.fullDesc <> OA.progDesc "Clean up and remove cache"
          )
        , ( "deploy"
          , pure Deploy
          , OA.fullDesc <> OA.progDesc "Upload/deploy your site"
           )
        , ( "rebuild"
          , pure Rebuild
          , OA.fullDesc <> OA.progDesc "Clean and build again"
          )
        ]


--------------------------------------------------------------------------------
-- | This is necessary because not everyone calls their program the same...
progName :: String
progName = unsafePerformIO getProgName
{-# NOINLINE progName #-}
