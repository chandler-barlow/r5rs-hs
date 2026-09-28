-- | Round-trip properties for individual lexical tokens, independent of
-- how they nest into a datum.
module Language.Scheme.LexerTest (tests) where

import Data.Text (Text)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Language.Scheme.Gen (genChar, genNumber, genString, genSymbol)
import Language.Scheme.Lexer
import Language.Scheme.Printer (renderChar, renderNumber, renderString)
import Text.Megaparsec (errorBundlePretty, eof, parse)

parseFull :: Parser a -> Text -> Either String a
parseFull p input = either (Left . errorBundlePretty) Right (parse (p <* eof) "<test>" input)

prop_number_roundtrip :: Property
prop_number_roundtrip = property $ do
    n <- forAll genNumber
    tripped <- evalEither (parseFull numberLit (renderNumber n))
    n === tripped

prop_symbol_roundtrip :: Property
prop_symbol_roundtrip = property $ do
    s <- forAll genSymbol
    tripped <- evalEither (parseFull identifierLit s)
    s === tripped

prop_char_roundtrip :: Property
prop_char_roundtrip = property $ do
    c <- forAll genChar
    tripped <- evalEither (parseFull charLit (renderChar c))
    c === tripped

prop_string_roundtrip :: Property
prop_string_roundtrip = property $ do
    s <- forAll genString
    tripped <- evalEither (parseFull stringLit (renderString s))
    s === tripped

prop_bool_roundtrip :: Property
prop_bool_roundtrip = property $ do
    b <- forAll Gen.bool
    let rendered = if b then "#t" else "#f"
    tripped <- evalEither (parseFull boolLit rendered)
    b === tripped

tests :: Group
tests = $$discover
