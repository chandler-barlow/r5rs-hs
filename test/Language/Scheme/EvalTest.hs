-- | Evaluator tests: fixed examples for each special form, a numeric
-- round-trip property tying the whole pipeline together, and a
-- deep-recursion regression proving proper tail calls actually work -
-- the R5RS-mandated property that's easiest to silently break.
module Language.Scheme.EvalTest (tests) where

import Control.Exception (Exception, SomeException, try)
import Data.Text (Text)
import Hedgehog
import Language.Scheme
import Language.Scheme.Gen (genNumber)
import Language.Scheme.Number (SchemeNumber (..), addNum)
import Language.Scheme.Printer (renderNumber)

runLast :: Text -> IO Value
runLast src = do
    env <- newGlobalEnv
    results <- interpret env src
    pure $ case results of
        [] -> Unspecified
        _ -> last results

tryError :: (Exception e) => IO a -> IO (Either e a)
tryError = try

prop_arithmetic :: Property
prop_arithmetic = property $ do
    v <- evalIO (runLast "(+ 1 2 (* 3 4) (- 10 5))")
    v === Number (ExactInteger 20)

prop_add_matches_addNum :: Property
prop_add_matches_addNum = property $ do
    a <- forAll genNumber
    b <- forAll genNumber
    let src = "(+ " <> renderNumber a <> " " <> renderNumber b <> ")"
    v <- evalIO (runLast src)
    v === Number (addNum a b)

prop_factorial :: Property
prop_factorial = property $ do
    v <-
        evalIO $
            runLast
                "(letrec ((fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))))\
                \  (fact 10))"
    v === Number (ExactInteger 3628800)

prop_named_let_tail_call_is_stack_safe :: Property
prop_named_let_tail_call_is_stack_safe = property $ do
    v <-
        evalIO $
            runLast
                "(let loop ((i 0) (acc 0))\
                \  (if (= i 200000) acc (loop (+ i 1) (+ acc i))))"
    v === Number (ExactInteger 19999900000)

prop_closures_and_set :: Property
prop_closures_and_set = property $ do
    v <-
        evalIO $
            runLast
                "(define (make-counter)\
                \  (let ((n 0))\
                \    (lambda () (set! n (+ n 1)) n)))\
                \(define c (make-counter))\
                \(c) (c) (c)"
    v === Number (ExactInteger 3)

prop_let_star_sees_earlier_bindings :: Property
prop_let_star_sees_earlier_bindings = property $ do
    v <- evalIO (runLast "(let* ((x 1) (y (+ x 1))) (+ x y))")
    v === Number (ExactInteger 3)

prop_quasiquote_splicing :: Property
prop_quasiquote_splicing = property $ do
    v <- evalIO (runLast "(let ((xs '(2 3))) `(1 ,@xs 4))")
    v === vlist (map (Number . ExactInteger) [1, 2, 3, 4])

prop_cond_else :: Property
prop_cond_else = property $ do
    v <- evalIO (runLast "(cond ((= 1 2) 'no) ((= 1 3) 'no) (else 'yes))")
    v === Symbol "yes"

prop_case_matches_datum :: Property
prop_case_matches_datum = property $ do
    v <- evalIO (runLast "(case (* 2 3) ((2 3 5 7) 'prime) ((1 4 6 8 9) 'composite) (else 'unknown))")
    v === Symbol "composite"

prop_and_or_short_circuit :: Property
prop_and_or_short_circuit = property $ do
    v1 <- evalIO (runLast "(and 1 2 #f 3)")
    v1 === Bool False
    v2 <- evalIO (runLast "(or #f #f 5 (car '()))")
    v2 === Number (ExactInteger 5)

prop_unbound_variable_errors :: Property
prop_unbound_variable_errors = property $ do
    result <- evalIO (tryError (runLast "totally-undefined-name") :: IO (Either SchemeError Value))
    case result of
        Left (UnboundVariable "totally-undefined-name") -> success
        other -> do
            footnote (show other)
            failure

prop_division_by_zero_errors :: Property
prop_division_by_zero_errors = property $ do
    result <- evalIO (tryError (runLast "(/ 1 0)") :: IO (Either SchemeError Value))
    result === Left DivideByZero

prop_arity_mismatch_errors :: Property
prop_arity_mismatch_errors = property $ do
    result <- evalIO (tryError (runLast "((lambda (x y) x) 1)") :: IO (Either SomeException Value))
    case result of
        Left _ -> success
        Right v -> do
            footnote (show v)
            failure

tests :: Group
tests = $$discover
