-- | The base set of primitive procedures, and 'newGlobalEnv' to build an
-- environment pre-populated with them. Deliberately a small, useful
-- core rather than full R5RS coverage - see the module haddock on
-- "Language.Scheme.Value" for what's out of scope (mutable pairs\/
-- strings\/vectors, object identity).
module Language.Scheme.Primitives
    ( newGlobalEnv
    ) where

import Control.Exception (catch, throwIO)
import Control.Monad (foldM)
import Data.List (transpose)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Data.Unique (hashUnique, newUnique)
import Language.Scheme.Eval (apply)
import Language.Scheme.Number
import Language.Scheme.Value

newGlobalEnv :: IO Env
newGlobalEnv = do
    env <- newEnv
    mapM_ (\(name, f) -> defineVar env name (Primitive name f)) primitiveTable
    pure env

primitiveTable :: [(Text, [Value] -> IO Value)]
primitiveTable =
    [ ("+", primAdd)
    , ("-", primSub)
    , ("*", primMul)
    , ("/", primDiv)
    , ("quotient", primIntegerOp "quotient" quot)
    , ("remainder", primIntegerOp "remainder" rem)
    , ("modulo", primIntegerOp "modulo" mod)
    , ("=", primCompare "=" (\a b -> compareNum a b == EQ))
    , ("<", primCompare "<" (\a b -> compareNum a b == LT))
    , (">", primCompare ">" (\a b -> compareNum a b == GT))
    , ("<=", primCompare "<=" (\a b -> compareNum a b /= GT))
    , (">=", primCompare ">=" (\a b -> compareNum a b /= LT))
    , ("zero?", primNumPred (\n -> compareNum n (ExactInteger 0) == EQ))
    , ("positive?", primNumPred (\n -> compareNum n (ExactInteger 0) == GT))
    , ("negative?", primNumPred (\n -> compareNum n (ExactInteger 0) == LT))
    , ("cons", primCons)
    , ("car", primCar)
    , ("cdr", primCdr)
    , ("list", pure . vlist)
    , ("length", primLength)
    , ("append", primAppend)
    , ("reverse", primReverse)
    , ("map", primMap)
    , ("for-each", primForEach)
    , ("apply", primApply)
    , ("eq?", prim2 "eq?" valueEqual)
    , ("eqv?", prim2 "eqv?" valueEqual)
    , ("equal?", prim2 "equal?" valueEqual)
    , ("not", primNot)
    , ("pair?", primPred isPair)
    , ("null?", primPred isNil)
    , ("symbol?", primPred isSymbol)
    , ("string?", primPred isString)
    , ("number?", primPred isNumber)
    , ("boolean?", primPred isBool)
    , ("procedure?", primPred isProcedure)
    , ("vector?", primPred isVector)
    , ("char?", primPred isChar)
    , ("display", primDisplay)
    , ("write", primWrite)
    , ("newline", primNewline)
    , ("error", primError)
    , ("call/cc", primCallCC)
    , ("call-with-current-continuation", primCallCC)
    ]

requireNumber :: Value -> IO SchemeNumber
requireNumber (Number n) = pure n
requireNumber v = throwIO (WrongType "number" v)

arityError :: Text -> String -> [Value] -> IO a
arityError name expected args = throwIO (ArityError name expected (length args))

primAdd, primMul :: [Value] -> IO Value
primAdd args = Number . foldl addNum (ExactInteger 0) <$> traverse requireNumber args
primMul args = Number . foldl mulNum (ExactInteger 1) <$> traverse requireNumber args

primSub :: [Value] -> IO Value
primSub [] = arityError "-" "at least 1" []
primSub [x] = Number . subNum (ExactInteger 0) <$> requireNumber x
primSub (x : xs) = do
    n <- requireNumber x
    ns <- traverse requireNumber xs
    pure (Number (foldl subNum n ns))

divChecked :: SchemeNumber -> SchemeNumber -> IO SchemeNumber
divChecked a b = either (const (throwIO DivideByZero)) pure (divNum a b)

primDiv :: [Value] -> IO Value
primDiv [] = arityError "/" "at least 1" []
primDiv [x] = Number <$> (requireNumber x >>= divChecked (ExactInteger 1))
primDiv (x : xs) = do
    n <- requireNumber x
    ns <- traverse requireNumber xs
    Number <$> foldM divChecked n ns

requireInteger :: Value -> IO Integer
requireInteger (Number (ExactInteger n)) = pure n
requireInteger v = throwIO (WrongType "integer" v)

primIntegerOp :: Text -> (Integer -> Integer -> Integer) -> [Value] -> IO Value
primIntegerOp _ f [x, y] = do
    a <- requireInteger x
    b <- requireInteger y
    if b == 0 then throwIO DivideByZero else pure (Number (ExactInteger (f a b)))
primIntegerOp name _ args = arityError name "exactly 2" args

primCompare :: Text -> (SchemeNumber -> SchemeNumber -> Bool) -> [Value] -> IO Value
primCompare _ _ [] = pure (Bool True)
primCompare _ _ [_] = pure (Bool True)
primCompare name cmp (x : y : rest) = do
    nx <- requireNumber x
    ny <- requireNumber y
    if cmp nx ny then primCompare name cmp (y : rest) else pure (Bool False)

primNumPred :: (SchemeNumber -> Bool) -> [Value] -> IO Value
primNumPred p [v] = Bool . p <$> requireNumber v
primNumPred _ args = arityError "numeric predicate" "exactly 1" args

primCons :: [Value] -> IO Value
primCons [a, b] = pure (Pair a b)
primCons args = arityError "cons" "exactly 2" args

primCar, primCdr :: [Value] -> IO Value
primCar [Pair a _] = pure a
primCar [v] = throwIO (WrongType "pair" v)
primCar args = arityError "car" "exactly 1" args
primCdr [Pair _ b] = pure b
primCdr [v] = throwIO (WrongType "pair" v)
primCdr args = arityError "cdr" "exactly 1" args

requireList :: Value -> IO [Value]
requireList v = maybe (throwIO (WrongType "list" v)) pure (valueToList v)

primLength :: [Value] -> IO Value
primLength [v] = Number . ExactInteger . fromIntegral . length <$> requireList v
primLength args = arityError "length" "exactly 1" args

primAppend :: [Value] -> IO Value
primAppend [] = pure Nil
primAppend args = case reverse args of
    (lastArg : rest) -> do
        lists <- traverse requireList (reverse rest)
        pure (foldr (flip (foldr Pair)) lastArg lists)
    [] -> pure Nil

primReverse :: [Value] -> IO Value
primReverse [v] = vlist . reverse <$> requireList v
primReverse args = arityError "reverse" "exactly 1" args

primMap :: [Value] -> IO Value
primMap (f : lists@(_ : _)) = do
    xss <- traverse requireList lists
    vlist <$> traverse (apply f) (transpose xss)
primMap args = arityError "map" "at least 2" args

primForEach :: [Value] -> IO Value
primForEach (f : lists@(_ : _)) = do
    xss <- traverse requireList lists
    Unspecified <$ traverse (apply f) (transpose xss)
primForEach args = arityError "for-each" "at least 2" args

primApply :: [Value] -> IO Value
primApply (f : args@(_ : _)) = case reverse args of
    (lastArg : initRev) -> do
        tailArgs <- requireList lastArg
        apply f (reverse initRev <> tailArgs)
    [] -> apply f []
primApply args = arityError "apply" "at least 2" args

prim2 :: Text -> (Value -> Value -> Bool) -> [Value] -> IO Value
prim2 _ f [a, b] = pure (Bool (f a b))
prim2 name _ args = arityError name "exactly 2" args

primNot :: [Value] -> IO Value
primNot [v] = pure (Bool (not (isTruthy v)))
primNot args = arityError "not" "exactly 1" args

isPair, isNil, isSymbol, isString, isNumber, isBool, isProcedure, isVector, isChar :: Value -> Bool
isPair (Pair _ _) = True
isPair _ = False
isNil Nil = True
isNil _ = False
isSymbol (Symbol _) = True
isSymbol _ = False
isString (String _) = True
isString _ = False
isNumber (Number _) = True
isNumber _ = False
isBool (Bool _) = True
isBool _ = False
isProcedure (Closure {}) = True
isProcedure (Primitive _ _) = True
isProcedure (Continuation _) = True
isProcedure _ = False
isVector (Vector _) = True
isVector _ = False
isChar (Character _) = True
isChar _ = False

primPred :: (Value -> Bool) -> [Value] -> IO Value
primPred p [v] = pure (Bool (p v))
primPred _ args = arityError "predicate" "exactly 1" args

primDisplay, primWrite :: [Value] -> IO Value
primDisplay [v] = Unspecified <$ TIO.putStr (displayValue v)
primDisplay args = arityError "display" "exactly 1" args
primWrite [v] = Unspecified <$ TIO.putStr (writeValue v)
primWrite args = arityError "write" "exactly 1" args

primNewline :: [Value] -> IO Value
primNewline [] = Unspecified <$ TIO.putStrLn ""
primNewline args = arityError "newline" "exactly 0" args

primError :: [Value] -> IO Value
primError (String msg : irritants) = throwIO (UserError msg irritants)
primError (v : _) = throwIO (WrongType "string" v)
primError [] = arityError "error" "at least 1" []

-- | Escape-only @call/cc@: tags a fresh 'Continuation', calls @f@ with
-- it, and catches the matching 'ContinuationInvoked' thrown when (if)
-- that continuation gets applied - anywhere in @f@'s dynamic extent,
-- including from inside nested calls. A non-matching tag (a
-- continuation captured by an /enclosing/ call\/cc) is re-thrown so it
-- keeps unwinding to its own frame; other 'SchemeError's pass through
-- untouched. See "Language.Scheme.Value" for what this doesn't support.
primCallCC :: [Value] -> IO Value
primCallCC [f] = do
    tag <- hashUnique <$> newUnique
    apply f [Continuation tag] `catch` \e -> case e of
        ContinuationInvoked tag' v | tag' == tag -> pure v
        _ -> throwIO e
primCallCC args = arityError "call/cc" "exactly 1" args
