--------------------------------------------------------------------------------
module Hexyll.Core.OldCompiler.Require
    ( Snapshot
    , save
    , saveSnapshot
    , load
    , loadSnapshot
    , loadBody
    , loadSnapshotBody
    , loadAll
    , loadAllSnapshots
    ) where


--------------------------------------------------------------------------------
import           Control.Monad                  (when)
import           Data.Binary                    (Binary)
import qualified Data.Set                       as S
import           Data.Typeable


--------------------------------------------------------------------------------
import           Hexyll.Core.OldCompiler.Internal
import           Hexyll.Core.Dependencies
import           Hexyll.Core.Identifier
import           Hexyll.Core.Identifier.Pattern hiding ( Pattern, match )
import           Hexyll.Core.Item
import           Hexyll.Core.Metadata           hiding ( Pattern )
import qualified Hexyll.Core.Metadata as Meta   ( Pattern (..) )
import           Hexyll.Core.OldStore           (Store)
import qualified Hexyll.Core.OldStore           as Store


--------------------------------------------------------------------------------
save :: (Binary a, Typeable a) => Store -> Item a -> IO ()
save store item = saveSnapshot store final item


--------------------------------------------------------------------------------
-- | Save a specific snapshot of an item, so you can load it later using
-- 'loadSnapshot'.
saveSnapshot :: (Binary a, Typeable a)
             => Store -> Snapshot -> Item a -> IO ()
saveSnapshot store snapshot item =
    Store.set store (key (itemIdentifier item) snapshot) (itemBody item)


--------------------------------------------------------------------------------
-- | Load an item compiled elsewhere. If the required item is not yet compiled,
-- the build system will take care of that automatically.
load :: (Binary a, Typeable a) => Identifier -> Compiler (Item a)
load id' = loadSnapshot id' final


--------------------------------------------------------------------------------
-- | Require a specific snapshot of an item.
loadSnapshot :: (Binary a, Typeable a)
             => Identifier -> Snapshot -> Compiler (Item a)
loadSnapshot id' snapshot = do
    store    <- compilerStore <$> compilerAsk
    universe <- compilerUniverse <$> compilerAsk

    -- Quick check for better error messages
    when (id' `S.notMember` universe) $ fail notFound

    compilerTellDependenciesCache [Dependency $ fromIdentifier id'] [id']
    compilerResult $ CompilerRequire (id', snapshot) $ do
        result <- compilerUnsafeIO $ Store.get store (key id' snapshot)
        case result of
            Store.NotFound      -> fail notFound
            Store.WrongType e r -> fail $ wrongType e r
            Store.Found x       -> return $ Item id' x
  where
    notFound =
        "Hexyll.Core.OldCompiler.Require.load: " ++ show id' ++
        " (snapshot " ++ snapshot ++ ") was not found in the cache, " ++
        "the cache might be corrupted or " ++
        "the item you are referring to might not exist"
    wrongType e r =
        "Hexyll.Core.OldCompiler.Require.load: " ++ show id' ++
        " (snapshot " ++ snapshot ++ ") was found in the cache, " ++
        "but does not have the right type: expected " ++ show e ++
        " but got " ++ show r


--------------------------------------------------------------------------------
-- | A shortcut for only requiring the body of an item.
--
-- > loadBody = fmap itemBody . load
loadBody :: (Binary a, Typeable a) => Identifier -> Compiler a
loadBody id' = loadSnapshotBody id' final


--------------------------------------------------------------------------------
-- | A shortcut for only requiring the body for a specific snapshot of an item
loadSnapshotBody :: (Binary a, Typeable a)
                 => Identifier -> Snapshot -> Compiler a
loadSnapshotBody id' snapshot = fmap itemBody $ loadSnapshot id' snapshot


--------------------------------------------------------------------------------
-- | This function allows you to 'load' a dynamic list of items
loadAll :: (Binary a, Typeable a) => Pattern -> Compiler [Item a]
loadAll pattern = loadAllSnapshots pattern final


--------------------------------------------------------------------------------
-- | Load a specific snapshot for each of dynamic list of items
loadAllSnapshots :: (Binary a, Typeable a)
                 => Pattern -> Snapshot -> Compiler [Item a]
loadAllSnapshots (Pattern pattern) snapshot = do
    matching <- getMatches $ Meta.Pattern pattern
    mapM (\i -> loadSnapshot i snapshot) matching


--------------------------------------------------------------------------------
key :: Identifier -> String -> [String]
key identifier snapshot =
    ["Hexyll.Core.OldCompiler.Require", show identifier, snapshot]


--------------------------------------------------------------------------------
final :: Snapshot
final = "_final"