-- | call\/cc tests: an escape that skips the rest of its expression, a
-- call\/cc that just returns normally, nested call\/cc unwinding past
-- an intervening frame to the one that actually captured the
-- continuation, and confirmation that invoking a continuation outside
-- its dynamic extent is a clean 'SchemeError' rather than a hang or an
-- unrelated crash (see the "escape-only" limitation documented on
-- 'Language.Scheme.Value.Continuation').
module Language.Scheme.CallCCTest (tests) where

import Control.Exception (Exception, try)
import Data.Text (Text)
import Hedgehog
import Language.Scheme
import Language.Scheme.Number (SchemeNumber (..))

runLast :: Text -> IO Value
runLast src = do
    env <- newGlobalEnv
    results <- interpret env src
    pure $ case results of
        [] -> Unspecified
        _ -> last results

tryError :: (Exception e) => IO a -> IO (Either e a)
tryError = try

prop_escape_skips_rest_of_expression :: Property
prop_escape_skips_rest_of_expression = property $ do
    v <- evalIO (runLast "(+ 1 (call/cc (lambda (k) (k 10) (error \"unreachable\"))))")
    v === Number (ExactInteger 11)

prop_normal_return_without_invoking_k :: Property
prop_normal_return_without_invoking_k = property $ do
    v <- evalIO (runLast "(call/cc (lambda (k) (+ 1 2)))")
    v === Number (ExactInteger 3)

prop_nested_escape_unwinds_to_capturing_frame :: Property
prop_nested_escape_unwinds_to_capturing_frame = property $ do
    v <-
        evalIO $
            runLast
                "(call/cc (lambda (outer)\
                \  (+ 1 (call/cc (lambda (inner)\
                \         (outer 100)\
                \         (inner 999))))))"
    v === Number (ExactInteger 100)

prop_early_exit_from_search :: Property
prop_early_exit_from_search = property $ do
    v <-
        evalIO $
            runLast
                "(define (first-even lst)\
                \  (call/cc (lambda (return)\
                \    (for-each (lambda (x) (if (= (modulo x 2) 0) (return x) #f)) lst)\
                \    #f)))\
                \(first-even '(1 3 5 6 7 8))"
    v === Number (ExactInteger 6)

prop_invoking_outside_extent_errors :: Property
prop_invoking_outside_extent_errors = property $ do
    result <-
        evalIO $
            tryError
                ( runLast
                    "(define saved #f)\
                    \(call/cc (lambda (k) (set! saved k) 1))\
                    \(saved 2)"
                ) ::
            PropertyT IO (Either SchemeError Value)
    case result of
        Left (ContinuationInvoked _ (Number (ExactInteger 2))) -> success
        other -> do
            footnote (show other)
            failure

tests :: Group
tests = $$discover
