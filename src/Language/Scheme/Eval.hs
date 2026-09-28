-- | The evaluator. R5RS section 3.5 requires implementations to be
-- "properly tail recursive" - a Scheme loop written as tail-recursive
-- calls must run in constant stack space. A direct @eval calls apply
-- calls eval@ recursion in Haskell would grow the Haskell stack once per
-- Scheme call, so tail positions instead return a 'Step' for a trampoline
-- ('eval') to loop on instead of recursing.
module Language.Scheme.Eval
    ( eval
    , apply
    ) where

import Control.Exception (throwIO)
import Control.Monad (foldM, zipWithM_)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Language.Scheme.Datum qualified as D
import Language.Scheme.Value

-- | Either a finished value, or a next expression/environment to
-- evaluate in tail position.
data Step = Done Value | Tail Env D.Datum

-- | Evaluate an expression to a value, trampolining through tail calls
-- instead of growing the Haskell stack.
eval :: Env -> D.Datum -> IO Value
eval = go
  where
    go env datum = do
        step <- step1 env datum
        case step of
            Done v -> pure v
            Tail env' datum' -> go env' datum'

-- | Apply a procedure to already-evaluated arguments. Unlike the
-- internal tail-call machinery, this always runs to completion - it's
-- the entry point primitives like @apply@\/@map@ use to call back into
-- Scheme.
apply :: Value -> [Value] -> IO Value
apply f args = do
    step <- applyStep f args
    case step of
        Done v -> pure v
        Tail env datum -> eval env datum

step1 :: Env -> D.Datum -> IO Step
step1 env = \case
    D.Symbol s -> do
        mv <- lookupVar env s
        maybe (throwIO (UnboundVariable s)) (pure . Done) mv
    D.Bool b -> pure (Done (Bool b))
    D.Number n -> pure (Done (Number n))
    D.Character c -> pure (Done (Character c))
    D.String s -> pure (Done (String s))
    D.Vector ds -> pure (Done (datumToValue (D.Vector ds)))
    D.Nil -> throwIO (SyntaxError "() is not a valid expression")
    D.Pair (D.Symbol "quote") (D.Pair d D.Nil) -> pure (Done (datumToValue d))
    D.Pair (D.Symbol "if") rest -> evalIf env rest
    D.Pair (D.Symbol "define") rest -> Done Unspecified <$ evalDefine env rest
    D.Pair (D.Symbol "set!") rest -> evalSet env rest
    D.Pair (D.Symbol "lambda") (D.Pair paramSpec body) -> do
        (params, rst) <- parseParams paramSpec
        bodyList <- requireProperList "lambda body" body
        pure (Done (Closure params rst bodyList env Nothing))
    D.Pair (D.Symbol "begin") body -> requireProperList "begin" body >>= tailSequence env
    D.Pair (D.Symbol "let") (D.Pair (D.Symbol loopName) (D.Pair bindings body)) ->
        namedLet env loopName bindings body
    D.Pair (D.Symbol "let") (D.Pair bindings body) -> evalLet env bindings body
    D.Pair (D.Symbol "let*") (D.Pair bindings body) -> evalLetStar env bindings body
    D.Pair (D.Symbol "letrec") (D.Pair bindings body) -> evalLetrec env bindings body
    D.Pair (D.Symbol "cond") clauses -> requireProperList "cond" clauses >>= evalCond env
    D.Pair (D.Symbol "case") (D.Pair keyExpr clauses) -> do
        key <- eval env keyExpr
        requireProperList "case" clauses >>= evalCase env key
    D.Pair (D.Symbol "and") exprs -> requireProperList "and" exprs >>= evalAnd env
    D.Pair (D.Symbol "or") exprs -> requireProperList "or" exprs >>= evalOr env
    D.Pair (D.Symbol "quasiquote") (D.Pair d D.Nil) -> Done <$> evalQuasiquote env 1 d
    D.Pair f args -> do
        fv <- eval env f
        argv <- requireProperList "procedure call" args >>= traverse (eval env)
        applyStep fv argv

applyStep :: Value -> [Value] -> IO Step
applyStep fv argv = case fv of
    Primitive _ f -> Done <$> f argv
    Closure params rst body cenv name -> do
        callEnv <- childEnv cenv
        bindParams (fromMaybe "#<anonymous>" name) params rst argv callEnv
        tailSequence callEnv body
    _ -> throwIO (NotApplicable fv)

bindParams :: Text -> [Text] -> Maybe Text -> [Value] -> Env -> IO ()
bindParams name params rst args env = do
    let nParams = length params
        nArgs = length args
    case rst of
        Nothing | nArgs /= nParams -> throwIO (ArityError name ("exactly " <> show nParams) nArgs)
        Just _ | nArgs < nParams -> throwIO (ArityError name ("at least " <> show nParams) nArgs)
        _ -> pure ()
    let (fixedArgs, restArgs) = splitAt nParams args
    zipWithM_ (defineVar env) params fixedArgs
    maybe (pure ()) (\restName -> defineVar env restName (vlist restArgs)) rst

-- | All but the last form of a body run for effect via the ordinary
-- (non-tail) 'eval'; the last form is returned as a 'Tail' step so the
-- caller's trampoline continues without growing the stack.
tailSequence :: Env -> [D.Datum] -> IO Step
tailSequence _ [] = pure (Done Unspecified)
tailSequence env [x] = pure (Tail env x)
tailSequence env (x : xs) = eval env x >> tailSequence env xs

evalIf :: Env -> D.Datum -> IO Step
evalIf env rest =
    requireProperList "if" rest >>= \case
        [c, t] -> do
            cv <- eval env c
            pure $ if isTruthy cv then Tail env t else Done Unspecified
        [c, t, e] -> do
            cv <- eval env c
            pure $ Tail env (if isTruthy cv then t else e)
        _ -> throwIO (SyntaxError "if: expected 2 or 3 subexpressions")

evalDefine :: Env -> D.Datum -> IO ()
evalDefine env = \case
    D.Pair (D.Symbol name) (D.Pair valExpr D.Nil) -> do
        v <- eval env valExpr
        defineVar env name (nameClosure name v)
    D.Pair (D.Symbol name) D.Nil -> defineVar env name Unspecified
    D.Pair (D.Pair (D.Symbol name) paramSpec) body -> do
        (params, rst) <- parseParams paramSpec
        bodyList <- requireProperList "define body" body
        defineVar env name (Closure params rst bodyList env (Just name))
    _ -> throwIO (SyntaxError "define: malformed")
  where
    nameClosure name v = case v of
        Closure ps rst body cenv Nothing -> Closure ps rst body cenv (Just name)
        _ -> v

evalSet :: Env -> D.Datum -> IO Step
evalSet env = \case
    D.Pair (D.Symbol name) (D.Pair valExpr D.Nil) -> do
        v <- eval env valExpr
        ok <- setVar env name v
        if ok then pure (Done Unspecified) else throwIO (UnboundVariable name)
    _ -> throwIO (SyntaxError "set!: malformed")

parseParams :: D.Datum -> IO ([Text], Maybe Text)
parseParams = \case
    D.Nil -> pure ([], Nothing)
    D.Symbol s -> pure ([], Just s)
    D.Pair (D.Symbol p) rst -> do
        (ps, r) <- parseParams rst
        pure (p : ps, r)
    _ -> throwIO (SyntaxError "lambda: malformed parameter list")

requireProperList :: Text -> D.Datum -> IO [D.Datum]
requireProperList ctx d = maybe (throwIO (SyntaxError (ctx <> ": expected a proper list"))) pure (D.properList d)

parseBindings :: D.Datum -> IO [(Text, D.Datum)]
parseBindings d = requireProperList "let bindings" d >>= traverse parseBinding
  where
    parseBinding (D.Pair (D.Symbol n) (D.Pair e D.Nil)) = pure (n, e)
    parseBinding _ = throwIO (SyntaxError "let: malformed binding")

evalLet :: Env -> D.Datum -> D.Datum -> IO Step
evalLet env bindingsD body = do
    bindings <- parseBindings bindingsD
    vals <- traverse (eval env . snd) bindings
    env' <- childEnv env
    zipWithM_ (defineVar env') (map fst bindings) vals
    requireProperList "let" body >>= tailSequence env'

evalLetStar :: Env -> D.Datum -> D.Datum -> IO Step
evalLetStar env bindingsD body = do
    bindings <- parseBindings bindingsD
    env' <- foldM step env bindings
    requireProperList "let*" body >>= tailSequence env'
  where
    step e (n, expr) = do
        v <- eval e expr
        e' <- childEnv e
        e' <$ defineVar e' n v

evalLetrec :: Env -> D.Datum -> D.Datum -> IO Step
evalLetrec env bindingsD body = do
    bindings <- parseBindings bindingsD
    env' <- childEnv env
    mapM_ (\(n, _) -> defineVar env' n Unspecified) bindings
    mapM_ (\(n, expr) -> eval env' expr >>= defineVar env' n) bindings
    requireProperList "letrec" body >>= tailSequence env'

namedLet :: Env -> Text -> D.Datum -> D.Datum -> IO Step
namedLet env loopName bindingsD body = do
    bindings <- parseBindings bindingsD
    initVals <- traverse (eval env . snd) bindings
    loopEnv <- childEnv env
    bodyList <- requireProperList "let" body
    let closure = Closure (map fst bindings) Nothing bodyList loopEnv (Just loopName)
    defineVar loopEnv loopName closure
    applyStep closure initVals

evalCond :: Env -> [D.Datum] -> IO Step
evalCond _ [] = pure (Done Unspecified)
evalCond env (clause : rest) =
    requireProperList "cond clause" clause >>= \case
        (D.Symbol "else" : body) -> tailSequence env body
        (test : body) -> do
            tv <- eval env test
            if isTruthy tv
                then if null body then pure (Done tv) else tailSequence env body
                else evalCond env rest
        [] -> throwIO (SyntaxError "cond: empty clause")

evalCase :: Env -> Value -> [D.Datum] -> IO Step
evalCase _ _ [] = pure (Done Unspecified)
evalCase env key (clause : rest) =
    requireProperList "case clause" clause >>= \case
        (D.Symbol "else" : body) -> tailSequence env body
        (datumsD : body) -> do
            datums <- requireProperList "case datums" datumsD
            if any (valueEqual key . datumToValue) datums
                then tailSequence env body
                else evalCase env key rest
        [] -> throwIO (SyntaxError "case: empty clause")

evalAnd :: Env -> [D.Datum] -> IO Step
evalAnd _ [] = pure (Done (Bool True))
evalAnd env [x] = pure (Tail env x)
evalAnd env (x : xs) = do
    v <- eval env x
    if isTruthy v then evalAnd env xs else pure (Done v)

evalOr :: Env -> [D.Datum] -> IO Step
evalOr _ [] = pure (Done (Bool False))
evalOr env [x] = pure (Tail env x)
evalOr env (x : xs) = do
    v <- eval env x
    if isTruthy v then pure (Done v) else evalOr env xs

-- | Quasiquote with correct nesting depth: an inner @quasiquote@
-- increments the depth an @unquote@\/@unquote-splicing@ must reach zero
-- against before it actually evaluates.
evalQuasiquote :: Env -> Int -> D.Datum -> IO Value
evalQuasiquote env depth = \case
    D.Pair (D.Symbol "unquote") (D.Pair d D.Nil)
        | depth == 1 -> eval env d
        | otherwise -> wrap "unquote" <$> evalQuasiquote env (depth - 1) d
    D.Pair (D.Symbol "quasiquote") (D.Pair d D.Nil) ->
        wrap "quasiquote" <$> evalQuasiquote env (depth + 1) d
    D.Pair (D.Pair (D.Symbol "unquote-splicing") (D.Pair d D.Nil)) rest
        | depth == 1 -> do
            spliced <- eval env d
            tailV <- evalQuasiquote env depth rest
            appendValue spliced tailV
        | otherwise -> do
            inner <- evalQuasiquote env (depth - 1) d
            tailV <- evalQuasiquote env depth rest
            pure (Pair (wrap "unquote-splicing" inner) tailV)
    D.Pair a b -> Pair <$> evalQuasiquote env depth a <*> evalQuasiquote env depth b
    D.Vector ds -> Vector <$> traverse (evalQuasiquote env depth) ds
    d -> pure (datumToValue d)
  where
    wrap tag v = Pair (Symbol tag) (Pair v Nil)

appendValue :: Value -> Value -> IO Value
appendValue v tailV = maybe (throwIO (WrongType "list" v)) (pure . foldr Pair tailV) (valueToList v)
