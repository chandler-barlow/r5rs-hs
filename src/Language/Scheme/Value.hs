-- | Runtime values, environments, and errors for the evaluator
-- (phase two). This is deliberately a plain-'IO' design, not a bespoke
-- monad: an interpreter's "effects" here are just mutable environment
-- frames and exceptions, both of which 'IO' already gives an embedding
-- host for free (no newtype to unwrap, 'catch' works directly on
-- 'SchemeError').
--
-- Bare-bones scope notes:
--
-- * Pairs, strings, and vectors are immutable Haskell values, not boxed
--   cells - @set-car!@\/@set-cdr!@\/@string-set!@\/@vector-set!@ don't
--   exist. Only variable bindings (via @set!@) are mutable.
-- * There's no object-identity model, so @eq?@, @eqv?@, and @equal?@ all
--   perform the same deep structural comparison ('valueEqual').
-- * @call/cc@ only supports escape (upward, one-shot) continuations, via
--   'ContinuationInvoked' - not full re-entrant\/multi-shot ones. A
--   continuation stays valid only while its @call/cc@ call is still on
--   the stack; invoking it after that raises an error instead of
--   resuming.
-- * @syntax-rules@ (in "Language.Scheme.Macro") is unhygienic: a
--   template-introduced binding can capture a same-named identifier from
--   the macro's use site.
module Language.Scheme.Value
    ( Value (..)
    , Env
    , newEnv
    , childEnv
    , lookupVar
    , defineVar
    , setVar
    , SchemeError (..)
    , isTruthy
    , vlist
    , valueToList
    , valueEqual
    , datumToValue
    , displayValue
    , writeValue
    ) where

import Control.Exception (Exception)
import Data.IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Language.Scheme.Datum qualified as D
import Language.Scheme.Number (SchemeNumber)
import Language.Scheme.Printer qualified as Printer

data Value
    = Symbol !Text
    | Bool !Bool
    | Number !SchemeNumber
    | Character !Char
    | String !Text
    | Pair Value Value
    | Nil
    | Vector [Value]
    | Unspecified
    | Closure
        { closureParams :: [Text]
        , closureRest :: Maybe Text
        , closureBody :: [D.Datum]
        , closureEnv :: Env
        , closureName :: Maybe Text
        }
    | Primitive Text ([Value] -> IO Value)
    | -- | A @syntax-rules@ transformer: literal identifiers, then
      -- (pattern, template) rules tried in order. See
      -- "Language.Scheme.Macro" - deliberately unhygienic.
      Macro [Text] [(D.Datum, D.Datum)]
    | -- | An escape-only continuation, tagged for 'ContinuationInvoked'
      -- to find its matching @call/cc@ frame. See the module haddock.
      Continuation Int

instance Show Value where
    show = Text.unpack . writeValue

-- | Structural equality (see 'valueEqual'); two closures or primitives
-- are never equal, since there's no object-identity model to compare
-- them by.
instance Eq Value where
    (==) = valueEqual

-- | A chain of mutable frames. Each frame is one 'IORef', so a closure
-- that captures an 'Env' sees every later @define@\/@set!@ against it -
-- exactly the sharing R5RS's lexical scoping requires.
data Env = Env
    { envFrame :: IORef (Map Text Value)
    , envParent :: Maybe Env
    }

newEnv :: IO Env
newEnv = Env <$> newIORef Map.empty <*> pure Nothing

childEnv :: Env -> IO Env
childEnv parent = Env <$> newIORef Map.empty <*> pure (Just parent)

lookupVar :: Env -> Text -> IO (Maybe Value)
lookupVar env name = do
    frame <- readIORef (envFrame env)
    case Map.lookup name frame of
        Just v -> pure (Just v)
        Nothing -> maybe (pure Nothing) (`lookupVar` name) (envParent env)

defineVar :: Env -> Text -> Value -> IO ()
defineVar env name val = modifyIORef' (envFrame env) (Map.insert name val)

-- | Mutate an existing binding, searching outward through parent frames.
-- Returns 'False' rather than creating a fresh binding when the variable
-- is unbound anywhere in the chain, matching R5RS's requirement that
-- @set!@ on an unbound variable is an error.
setVar :: Env -> Text -> Value -> IO Bool
setVar env name val = do
    frame <- readIORef (envFrame env)
    if Map.member name frame
        then True <$ modifyIORef' (envFrame env) (Map.insert name val)
        else maybe (pure False) (\parent -> setVar parent name val) (envParent env)

data SchemeError
    = UnboundVariable Text
    | NotApplicable Value
    | WrongType Text Value
    | ArityError Text String Int
    | DivideByZero
    | SyntaxError Text
    | UserError Text [Value]
    | -- | Thrown when a captured continuation is invoked (see
      -- 'Continuation'). A @call/cc@ frame catches this and returns the
      -- carried value when the tag is its own; otherwise it re-throws,
      -- letting the exception unwind past intervening frames to the one
      -- that captured it. If it escapes 'interpret' entirely, the
      -- continuation was invoked outside its dynamic extent - this
      -- implementation only supports escape (upward, one-shot)
      -- continuations, not full re-entrant ones.
      ContinuationInvoked Int Value
    deriving stock (Show, Eq)

instance Exception SchemeError

isTruthy :: Value -> Bool
isTruthy (Bool False) = False
isTruthy _ = True

vlist :: [Value] -> Value
vlist = foldr Pair Nil

valueToList :: Value -> Maybe [Value]
valueToList Nil = Just []
valueToList (Pair a b) = (a :) <$> valueToList b
valueToList _ = Nothing

valueEqual :: Value -> Value -> Bool
valueEqual a b = case (a, b) of
    (Bool x, Bool y) -> x == y
    (Symbol x, Symbol y) -> x == y
    (Number x, Number y) -> x == y
    (Character x, Character y) -> x == y
    (String x, String y) -> x == y
    (Nil, Nil) -> True
    (Unspecified, Unspecified) -> True
    (Pair x1 y1, Pair x2 y2) -> valueEqual x1 x2 && valueEqual y1 y2
    (Vector xs, Vector ys) -> length xs == length ys && and (zipWith valueEqual xs ys)
    (Continuation x, Continuation y) -> x == y
    _ -> False

-- | A quoted (or self-evaluating) datum, taken as literal data.
datumToValue :: D.Datum -> Value
datumToValue = \case
    D.Symbol s -> Symbol s
    D.Bool b -> Bool b
    D.Number n -> Number n
    D.Character c -> Character c
    D.String s -> String s
    D.Pair a b -> Pair (datumToValue a) (datumToValue b)
    D.Nil -> Nil
    D.Vector ds -> Vector (map datumToValue ds)

writeValue :: Value -> Text
writeValue = \case
    Symbol s -> s
    Bool True -> "#t"
    Bool False -> "#f"
    Number n -> Printer.renderNumber n
    Character c -> Printer.renderChar c
    String s -> Printer.renderString s
    Nil -> "()"
    Pair a b -> "(" <> writePair a b <> ")"
    Vector vs -> "#(" <> Text.unwords (map writeValue vs) <> ")"
    Unspecified -> ""
    Closure {closureName = Just n} -> "#<procedure:" <> n <> ">"
    Closure {closureName = Nothing} -> "#<procedure>"
    Primitive name _ -> "#<procedure:" <> name <> ">"
    Macro {} -> "#<macro>"
    Continuation _ -> "#<continuation>"

writePair :: Value -> Value -> Text
writePair a b = case b of
    Nil -> writeValue a
    Pair c d -> writeValue a <> " " <> writePair c d
    _ -> writeValue a <> " . " <> writeValue b

-- | Like 'writeValue', but strings and characters render as raw content
-- rather than re-readable syntax (R5RS's @display@ vs @write@).
displayValue :: Value -> Text
displayValue = \case
    String s -> s
    Character c -> Text.singleton c
    Pair a b -> "(" <> displayPair a b <> ")"
    Vector vs -> "#(" <> Text.unwords (map displayValue vs) <> ")"
    v -> writeValue v

displayPair :: Value -> Value -> Text
displayPair a b = case b of
    Nil -> displayValue a
    Pair c d -> displayValue a <> " " <> displayPair c d
    _ -> displayValue a <> " . " <> displayValue b
