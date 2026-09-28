-- | syntax-rules tests: a non-ellipsis macro, a recursive ellipsis
-- macro (my-or), and a macro with two independent ellipsis groups in
-- one pattern\/template (a my-let reimplementation) - the case most
-- likely to break a hand-rolled ellipsis matcher.
module Language.Scheme.MacroTest (tests) where

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

prop_simple_macro :: Property
prop_simple_macro = property $ do
    v <-
        evalIO $
            runLast
                "(define-syntax my-if\
                \  (syntax-rules ()\
                \    ((_ c t e) (cond (c t) (else e)))))\
                \(my-if #t 'yes 'no)"
    v === Symbol "yes"

prop_recursive_ellipsis_macro :: Property
prop_recursive_ellipsis_macro = property $ do
    v <-
        evalIO $
            runLast
                "(define-syntax my-or\
                \  (syntax-rules ()\
                \    ((_) #f)\
                \    ((_ e) e)\
                \    ((_ e1 e2 ...) (let ((t e1)) (if t t (my-or e2 ...))))))\
                \(my-or #f #f 3 (error \"should not reach here\"))"
    v === Number (ExactInteger 3)

prop_two_ellipsis_groups :: Property
prop_two_ellipsis_groups = property $ do
    v <-
        evalIO $
            runLast
                "(define-syntax my-let\
                \  (syntax-rules ()\
                \    ((_ ((name val) ...) body ...)\
                \     ((lambda (name ...) body ...) val ...))))\
                \(my-let ((x 1) (y 2)) (define z (+ x y)) z)"
    v === Number (ExactInteger 3)

tests :: Group
tests = $$discover
