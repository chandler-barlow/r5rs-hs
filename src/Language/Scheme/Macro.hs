-- | @syntax-rules@ pattern matching and template expansion.
--
-- Bare-bones scope: this is deliberately /not/ hygienic - a template
-- that introduces a binding (e.g. a @let@-bound temporary) can capture a
-- same-named variable from the macro's use site, exactly the bug
-- hygiene exists to prevent. It also doesn't support: the @(... escape)@
-- form for embedding a literal ellipsis in a template; a custom ellipsis
-- identifier; @let-syntax@\/@letrec-syntax@ (only top-level
-- @define-syntax@); or fixed pattern elements coming /after/ an
-- ellipsis (only "@pat ...@ as the rest of the list" is supported,
-- which covers the overwhelming majority of real macros). Nested
-- ellipsis (@(pat ...) ...@) does work, via 'Binding' nesting.
module Language.Scheme.Macro
    ( parseSyntaxRules
    , expandMacro
    ) where

import Control.Exception (throwIO)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Language.Scheme.Datum qualified as D
import Language.Scheme.Value (SchemeError (..), Value (..))

-- | What a pattern variable captured: either a single datum, or (under
-- an ellipsis) a list of what each repetition captured - nested once
-- per level of ellipsis enclosing the variable.
data Binding = BOne D.Datum | BMany [Binding]

type Bindings = Map Text Binding

-- | Parse @(syntax-rules (literal ...) (pattern template) ...)@ into a
-- 'Macro' value.
parseSyntaxRules :: D.Datum -> IO Value
parseSyntaxRules (D.Pair (D.Symbol "syntax-rules") (D.Pair litsD rulesD)) = do
    literals <- requireProperList "syntax-rules literals" litsD >>= traverse requireSymbol
    rules <- requireProperList "syntax-rules rules" rulesD >>= traverse parseRule
    pure (Macro literals rules)
  where
    requireSymbol (D.Symbol s) = pure s
    requireSymbol _ = throwIO (SyntaxError "syntax-rules: each literal must be a symbol")
    parseRule (D.Pair pat (D.Pair tmpl D.Nil)) = pure (pat, tmpl)
    parseRule _ = throwIO (SyntaxError "syntax-rules: malformed rule, expected (pattern template)")
parseSyntaxRules _ = throwIO (SyntaxError "define-syntax: expected (syntax-rules (literals...) rules...)")

requireProperList :: Text -> D.Datum -> IO [D.Datum]
requireProperList ctx d = maybe (throwIO (SyntaxError (ctx <> ": expected a proper list"))) pure (D.properList d)

-- | Expand one macro use: @args@ is everything after the macro keyword,
-- e.g. for @(my-macro a b)@, @args@ is the datum @(a b)@.
expandMacro :: Text -> [Text] -> [(D.Datum, D.Datum)] -> D.Datum -> IO D.Datum
expandMacro name literals rules args = go rules
  where
    go [] = throwIO (SyntaxError (name <> ": no matching syntax-rules pattern"))
    go ((pat, tmpl) : rest) = case matchPattern literals (patternArgs pat) args of
        Just binds -> pure (instantiate binds tmpl)
        Nothing -> go rest
    -- The pattern's first element (the macro keyword position) is never
    -- matched against, per R5RS.
    patternArgs (D.Pair _ r) = r
    patternArgs _ = D.Nil

matchPattern :: [Text] -> D.Datum -> D.Datum -> Maybe Bindings
matchPattern literals pat input = case pat of
    D.Symbol "_" -> Just Map.empty
    D.Symbol s
        | s `elem` literals -> if input == D.Symbol s then Just Map.empty else Nothing
        | otherwise -> Just (Map.singleton s (BOne input))
    D.Pair sub (D.Pair (D.Symbol "...") D.Nil) -> do
        elems <- D.properList input
        elemBindings <- traverse (matchPattern literals sub) elems
        let vars = patternVars literals sub
        pure (Map.fromList [(v, BMany (map (Map.findWithDefault (BOne D.Nil) v) elemBindings)) | v <- vars])
    D.Pair p1 prest -> case input of
        D.Pair i1 irest -> Map.union <$> matchPattern literals p1 i1 <*> matchPattern literals prest irest
        _ -> Nothing
    D.Vector ps -> case input of
        D.Vector is -> matchPattern literals (D.list ps) (D.list is)
        _ -> Nothing
    _ -> if pat == input then Just Map.empty else Nothing

-- | Identifiers a pattern binds - everything except literals, @_@, and
-- @...@ itself.
patternVars :: [Text] -> D.Datum -> [Text]
patternVars literals = \case
    D.Symbol "_" -> []
    D.Symbol "..." -> []
    D.Symbol s
        | s `elem` literals -> []
        | otherwise -> [s]
    D.Pair a b -> patternVars literals a <> patternVars literals b
    D.Vector ds -> concatMap (patternVars literals) ds
    _ -> []

instantiate :: Bindings -> D.Datum -> D.Datum
instantiate binds tmpl = case tmpl of
    D.Symbol s -> case Map.lookup s binds of
        Just (BOne d) -> d
        _ -> tmpl
    D.Pair sub (D.Pair (D.Symbol "...") rest) ->
        let vars = [v | v <- templateVars sub, isMany (Map.lookup v binds)]
            n = case vars of
                (v : _) -> case binds Map.! v of BMany bs -> length bs; BOne _ -> 0
                [] -> 0
            expanded = [instantiate (sliceBindings vars i binds) sub | i <- [0 .. n - 1]]
         in foldr D.Pair (instantiate binds rest) expanded
    D.Pair a b -> D.Pair (instantiate binds a) (instantiate binds b)
    D.Vector ds -> D.Vector (fromMaybe [] (D.properList (instantiate binds (D.list ds))))
    _ -> tmpl
  where
    isMany (Just (BMany _)) = True
    isMany _ = False

templateVars :: D.Datum -> [Text]
templateVars = \case
    D.Symbol s -> [s]
    D.Pair a b -> templateVars a <> templateVars b
    D.Vector ds -> concatMap templateVars ds
    _ -> []

-- | Unwrap one level of ellipsis for the @i@-th repetition: each
-- 'BMany'-bound variable in @vars@ becomes its @i@-th element; every
-- other binding (including non-repeated variables still in scope) is
-- left as-is.
sliceBindings :: [Text] -> Int -> Bindings -> Bindings
sliceBindings vars i binds = foldr slice1 binds vars
  where
    slice1 v m = case Map.lookup v binds of
        Just (BMany bs) | i < length bs -> Map.insert v (bs !! i) m
        _ -> m
