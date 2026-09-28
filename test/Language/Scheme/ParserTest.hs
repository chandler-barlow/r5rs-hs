-- | Datum-level round-trip property, plus a few fixed examples for cases
-- that are awkward to generate (dotted pairs, comments, quote
-- abbreviations, radix prefixes).
module Language.Scheme.ParserTest (tests) where

import Data.Text (Text)
import Hedgehog
import Language.Scheme.Datum
import Language.Scheme.Gen (genDatum)
import Language.Scheme.Number (SchemeNumber (..), mkRational)
import Language.Scheme.Parser (parseDatum, parseProgram)
import Language.Scheme.Printer (render)
import Text.Megaparsec (errorBundlePretty)

parseDatumText :: Text -> Either String Datum
parseDatumText input = either (Left . errorBundlePretty) Right (parseDatum "<test>" input)

parseProgramText :: Text -> Either String [Datum]
parseProgramText input = either (Left . errorBundlePretty) Right (parseProgram "<test>" input)

prop_datum_roundtrip :: Property
prop_datum_roundtrip = property $ do
    d <- forAll genDatum
    tripped <- evalEither (parseDatumText (render d))
    d === tripped

prop_dotted_pair :: Property
prop_dotted_pair = property $
    parseDatumText "(1 . 2)" === Right (Pair (Number (ExactInteger 1)) (Number (ExactInteger 2)))

prop_proper_list :: Property
prop_proper_list = property $
    parseDatumText "(1 2 3)"
        === Right (list (map (Number . ExactInteger) [1, 2, 3]))

prop_quote_abbreviations :: Property
prop_quote_abbreviations = property $ do
    parseDatumText "'a" === Right (list [Symbol "quote", Symbol "a"])
    parseDatumText "`a" === Right (list [Symbol "quasiquote", Symbol "a"])
    parseDatumText ",a" === Right (list [Symbol "unquote", Symbol "a"])
    parseDatumText ",@a" === Right (list [Symbol "unquote-splicing", Symbol "a"])

prop_vector :: Property
prop_vector = property $
    parseDatumText "#(1 2 3)" === Right (Vector (map (Number . ExactInteger) [1, 2, 3]))

prop_negative_rational :: Property
prop_negative_rational = property $
    parseDatumText "-3/4" === Right (Number (mkRational (-3) 4))

prop_hex_literal :: Property
prop_hex_literal = property $
    parseDatumText "#xFF" === Right (Number (ExactInteger 255))

prop_line_comment_ignored :: Property
prop_line_comment_ignored = property $ do
    result <- evalEither (parseProgramText "1 ; this is a comment\n2")
    result === [Number (ExactInteger 1), Number (ExactInteger 2)]

prop_string_escapes :: Property
prop_string_escapes = property $
    parseDatumText "\"a\\\"b\\\\c\"" === Right (String "a\"b\\c")

tests :: Group
tests = $$discover
