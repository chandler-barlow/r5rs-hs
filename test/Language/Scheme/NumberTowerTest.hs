-- | Fixed examples for the complex-number layer of the numeric tower
-- and the extended string escapes: literal parsing, arithmetic
-- (including the case where multiplying two complex numbers collapses
-- back to a real), the exactness\/type predicates, conversions, and the
-- restriction that ordering comparisons reject complex numbers.
module Language.Scheme.NumberTowerTest (tests) where

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

prop_complex_literal_parses :: Property
prop_complex_literal_parses = property $ do
    v <- evalIO (runLast "3+4i")
    v === Number (ExactComplex 3 4)

prop_pure_imaginary_literal :: Property
prop_pure_imaginary_literal = property $ do
    v <- evalIO (runLast "-i")
    v === Number (ExactComplex 0 (-1))

prop_complex_addition :: Property
prop_complex_addition = property $ do
    v <- evalIO (runLast "(+ 1+2i 3+4i)")
    v === Number (ExactComplex 4 6)

prop_imaginary_unit_squared_is_real :: Property
prop_imaginary_unit_squared_is_real = property $ do
    v <- evalIO (runLast "(* 0+1i 0+1i)")
    v === Number (ExactInteger (-1))

prop_real_zero_imaginary_collapses :: Property
prop_real_zero_imaginary_collapses = property $ do
    v <- evalIO (runLast "(make-rectangular 5 0)")
    v === Number (ExactInteger 5)

prop_real_part_imag_part :: Property
prop_real_part_imag_part = property $ do
    re <- evalIO (runLast "(real-part 3+4i)")
    re === Number (ExactInteger 3)
    im <- evalIO (runLast "(imag-part 3+4i)")
    im === Number (ExactInteger 4)

prop_exact_inexact_predicates :: Property
prop_exact_inexact_predicates = property $ do
    a <- evalIO (runLast "(exact? 1/2)")
    a === Bool True
    b <- evalIO (runLast "(inexact? 1.5)")
    b === Bool True
    c <- evalIO (runLast "(exact? 3+4i)")
    c === Bool True
    d <- evalIO (runLast "(inexact? 1.0+2i)")
    d === Bool True

prop_tower_type_predicates :: Property
prop_tower_type_predicates = property $ do
    a <- evalIO (runLast "(integer? 4)")
    a === Bool True
    b <- evalIO (runLast "(integer? 4.5)")
    b === Bool False
    c <- evalIO (runLast "(rational? 1/2)")
    c === Bool True
    d <- evalIO (runLast "(real? 3+4i)")
    d === Bool False
    e <- evalIO (runLast "(complex? 3+4i)")
    e === Bool True
    f <- evalIO (runLast "(real? 5)")
    f === Bool True

prop_exact_inexact_conversion :: Property
prop_exact_inexact_conversion = property $ do
    v <- evalIO (runLast "(exact->inexact 1/2)")
    v === Number (InexactReal 0.5)
    w <- evalIO (runLast "(inexact->exact 0.5)")
    w === Number (ExactRational 0.5)

prop_numeric_equality_crosses_exactness :: Property
prop_numeric_equality_crosses_exactness = property $ do
    v <- evalIO (runLast "(= 3 3.0)")
    v === Bool True

prop_ordering_rejects_complex :: Property
prop_ordering_rejects_complex = property $ do
    result <- evalIO (tryError (runLast "(< 1+2i 3)") :: IO (Either SchemeError Value))
    case result of
        Left (WrongType "real number" _) -> success
        other -> do
            footnote (show other)
            failure

prop_string_escapes_roundtrip :: Property
prop_string_escapes_roundtrip = property $ do
    v <- evalIO (runLast "\"a\\nb\\tc\\r\\a\\b\"")
    v === String "a\nb\tc\r\a\b"

tests :: Group
tests = $$discover
