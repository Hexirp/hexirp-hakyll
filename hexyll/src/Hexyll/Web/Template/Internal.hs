{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Hexyll.Web.Template.Internal
    ( Template (..)
    , template
    , templateBodyCompiler
    , templateCompiler
    , applyTemplate
    , loadAndApplyTemplate
    , applyAsTemplate
    , readTemplate
    , compileTemplateItem
    , unsafeReadTemplateFile

    , module Hexyll.Web.Template.Internal.Element
    , module Hexyll.Web.Template.Internal.Trim
    ) where


--------------------------------------------------------------------------------
import           Control.Monad.Except                 (catchError)
import           Data.Binary                          (Binary)
import           Data.List                            (intercalate)
import qualified Data.List.NonEmpty                   as NonEmpty
import           Data.Typeable                        (Typeable)
import           GHC.Exts                             (IsString (..))
import           GHC.Generics                         (Generic)


--------------------------------------------------------------------------------
import           Hexyll.Core.OldCompiler
import           Hexyll.Core.OldCompiler.Internal
import           Hexyll.Core.Identifier
import           Hexyll.Core.Item
import           Hexyll.Core.OldWritable
import           Hexyll.Web.Template.Context
import           Hexyll.Web.Template.Internal.Element
import           Hexyll.Web.Template.Internal.Trim


--------------------------------------------------------------------------------
-- | Datatype used for template substitutions.
data Template = Template
    { tplElements :: [TemplateElement]
    , tplOrigin   :: FilePath  -- Only for error messages.
    } deriving (Show, Eq, Generic, Binary, Typeable)


--------------------------------------------------------------------------------
instance Writable Template where
    -- Writing a template is impossible
    write _ _ = return ()


--------------------------------------------------------------------------------
instance IsString Template where
    fromString = readTemplate


--------------------------------------------------------------------------------
-- | Wrap the constructor to ensure trim is called.
template :: FilePath -> [TemplateElement] -> Template
template p = flip Template p . trim


--------------------------------------------------------------------------------
-- | Parse a string into a template.
-- You should prefer 'compileTemplateItem' over this.
readTemplate :: String -> Template
readTemplate = either error (template origin) . parseTemplateElemsFile origin
  where
    origin = "{literal}"
{-# DEPRECATED readTemplate "Use templateCompiler instead" #-}

--------------------------------------------------------------------------------
-- | Parse an item body into a template.
-- Provides useful error messages in the 'Compiler' monad.
compileTemplateItem :: Item String -> Compiler Template
compileTemplateItem item = let file = itemIdentifier item
                           in compileTemplateFile file (itemBody item)

--------------------------------------------------------------------------------
compileTemplateFile :: Identifier -> String -> Compiler Template
compileTemplateFile file = either fail (return . template origin)
                         . parseTemplateElemsFile origin
  where
    origin = show file

--------------------------------------------------------------------------------
-- | Read a template, without metadata header
templateBodyCompiler :: Compiler (Item Template)
templateBodyCompiler = cached "Hexyll.Web.Template.templateBodyCompiler" $ do
    item <- getResourceBody
    file <- getUnderlying
    traverse (compileTemplateFile file) item

--------------------------------------------------------------------------------
-- | Read complete file contents as a template
templateCompiler :: Compiler (Item Template)
templateCompiler = cached "Hexyll.Web.Template.templateCompiler" $ do
    item <- getResourceString
    file <- getUnderlying
    traverse (compileTemplateFile file) item


--------------------------------------------------------------------------------
-- | Interpolate template expressions from context values in a page
applyTemplate :: Template                -- ^ Template
              -> Context a               -- ^ Context
              -> Item a                  -- ^ Page
              -> Compiler (Item String)  -- ^ Resulting item
applyTemplate tpl context item = do
    body <- applyTemplate' (tplElements tpl) context item `catchError` handler
    return $ setItemBody body item
  where
    tplName = tplOrigin tpl
    itemName = show $ itemIdentifier item
    handler es = fail $ "Hexyll.Web.Template.applyTemplate: Failed to " ++
        (if tplName == itemName
          then "interpolate template in item " ++ itemName
          else "apply template " ++ tplName ++ " to item " ++ itemName) ++
        ":\n" ++ intercalate ",\n" es



--------------------------------------------------------------------------------
applyTemplate'
    :: forall a.
       [TemplateElement] -- ^ Unwrapped Template
    -> Context a         -- ^ Context
    -> Item a            -- ^ Page
    -> Compiler String   -- ^ Resulting item
applyTemplate' tes context x = go tes
  where
    context' :: String -> [String] -> Item a -> Compiler ContextField
    context' = unContext (context `mappend` missingField)

    go = fmap concat . mapM applyElem

    ---------------------------------------------------------------------------

    applyElem :: TemplateElement -> Compiler String

    applyElem TrimL = trimError

    applyElem TrimR = trimError

    applyElem (Chunk c) = return c

    applyElem (Expr e) = withErrorMessage evalMsg (applyStringExpr typeMsg e)
      where
        evalMsg = "In expr '$" ++ show e ++ "$'"
        typeMsg = "expr '$" ++ show e ++ "$'"

    applyElem Escaped = return "$"

    applyElem (If e t mf) = compilerTry (applyExpr e) >>= handle
      where
        f = maybe (return "") go mf
        handle (Right _)                      = go t
        handle (Left (CompilationNoResult _)) = f
        handle (Left (CompilationFailure es)) = debug (NonEmpty.toList es) >> f
        debug = compilerDebugEntries ("Hexyll.Web.Template.applyTemplate: " ++
            "[ERROR] in 'if' condition on expr '" ++ show e ++ "':")

    applyElem (For e b s) = withErrorMessage headMsg (applyExpr e) >>= \cf -> case cf of
        EmptyField     -> expected "list" "boolean" typeMsg
        StringField _  -> expected "list" "string" typeMsg
        ListField c xs -> withErrorMessage bodyMsg $ do
            sep <- maybe (return "") go s
            bs  <- mapM (applyTemplate' b c) xs
            return $ intercalate sep bs
      where
        headMsg = "In expr '$for(" ++ show e ++ ")$'"
        typeMsg = "loop expr '" ++ show e ++ "'"
        bodyMsg = "In loop context of '$for(" ++ show e ++ ")$'"

    applyElem (Partial e) = withErrorMessage headMsg $
        applyStringExpr typeMsg e >>= \p ->
        withErrorMessage inclMsg $ do
            tpl' <- loadBody (ufromFilePath p)
            itemBody <$> applyTemplate tpl' context x
      where
        headMsg = "In expr '$partial(" ++ show e ++ ")$'"
        typeMsg = "partial expr '" ++ show e ++ "'"
        inclMsg = "In inclusion of '$partial(" ++ show e ++ ")$'"

    ---------------------------------------------------------------------------

    applyExpr :: TemplateExpr -> Compiler ContextField

    applyExpr (Ident (TemplateKey k)) = context' k [] x

    applyExpr (Call (TemplateKey k) args) = do
        args' <- mapM (\e -> applyStringExpr (typeMsg e) e) args
        context' k args' x
      where
        typeMsg e = "argument '" ++ show e ++ "'"

    applyExpr (StringLiteral s) = return (StringField s)

    ----------------------------------------------------------------------------

    applyStringExpr :: String -> TemplateExpr -> Compiler String
    applyStringExpr msg expr =
        applyExpr expr >>= getString
      where
        getString EmptyField      = expected "string" "boolean" msg
        getString (StringField s) = return s
        getString (ListField _ _) = expected "string" "list" msg

    expected typ act expr = fail $ unwords ["Hexyll.Web.Template.applyTemplate:",
        "expected", typ, "but got", act, "for", expr]

    -- expected to never happen with all templates constructed by 'template'
    trimError = fail $
        "Hexyll.Web.Template.applyTemplate: template not fully trimmed."


--------------------------------------------------------------------------------
-- | The following pattern is so common:
--
-- > tpl <- loadBody "templates/foo.html"
-- > someCompiler
-- >     >>= applyTemplate tpl context
--
-- That we have a single function which does this:
--
-- > someCompiler
-- >     >>= loadAndApplyTemplate "templates/foo.html" context
loadAndApplyTemplate :: Identifier              -- ^ Template identifier
                     -> Context a               -- ^ Context
                     -> Item a                  -- ^ Page
                     -> Compiler (Item String)  -- ^ Resulting item
loadAndApplyTemplate identifier context item = do
    tpl <- loadBody identifier
    applyTemplate tpl context item


--------------------------------------------------------------------------------
-- | It is also possible that you want to substitute @$key$@s within the body of
-- an item. This function does that by interpreting the item body as a template,
-- and then applying it to itself.
applyAsTemplate :: Context String          -- ^ Context
                -> Item String             -- ^ Item and template
                -> Compiler (Item String)  -- ^ Resulting item
applyAsTemplate context item = do
    tpl <- compileTemplateItem item
    applyTemplate tpl context item


--------------------------------------------------------------------------------
unsafeReadTemplateFile :: FilePath -> Compiler Template
unsafeReadTemplateFile file = do
    tpl <- unsafeCompiler $ readFile file
    compileTemplateFile (ufromFilePath file) tpl
{-# DEPRECATED unsafeReadTemplateFile "Use templateCompiler" #-}
