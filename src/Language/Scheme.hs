-- | The embedding entry point: read and evaluate R5RS source against an
-- environment, or hand-build values and call into it directly. A host
-- typically does:
--
-- > env <- newGlobalEnv
-- > defineVar env "host-fn" (Primitive "host-fn" myHaskellFunction)
-- > results <- interpret env sourceText
--
-- Everything here runs in plain 'IO' - no monad transformer to unwrap,
-- 'Control.Exception.catch' works directly against 'SchemeError'.
module Language.Scheme
    ( -- * Values and environments
      Value (..)
    , Env
    , newGlobalEnv
    , defineVar
    , lookupVar
    , SchemeError (..)
    , vlist
    , valueToList
    , valueEqual
    , isTruthy

      -- * Reading and evaluating
    , Datum
    , ReadError
    , parseProgram
    , eval
    , apply
    , interpret

      -- * Rendering values
    , writeValue
    , displayValue
    ) where

import Control.Exception (throwIO)
import Data.Text (Text)
import Data.Text qualified as Text
import Language.Scheme.Datum (Datum)
import Language.Scheme.Eval (apply, eval)
import Language.Scheme.Parser (ReadError, parseProgram)
import Language.Scheme.Primitives (newGlobalEnv)
import Language.Scheme.Value
import Text.Megaparsec (errorBundlePretty)

-- | Parse and evaluate a whole program, in order, returning each
-- top-level result. Throws 'ReadError' wrapped as a 'SyntaxError' on a
-- parse failure, or 'SchemeError' on an evaluation failure.
interpret :: Env -> Text -> IO [Value]
interpret env src = case parseProgram "<embedded>" src of
    Left err -> throwIO (SyntaxError (Text.pack (errorBundlePretty err)))
    Right datums -> traverse (eval env) datums
