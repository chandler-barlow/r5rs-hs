-- | Renders a 'Datum' back to R5RS external representation. Exists mainly
-- so the reader can be tested against a round trip (@parse . render ==
-- Right@); it is a minimal @write@-style printer, not a full
-- display\/write pair.
module Language.Scheme.Printer (render, renderNumber, renderChar, renderString) where

import Data.Ratio (denominator, numerator)
import Data.Text (Text)
import Data.Text qualified as Text
import Language.Scheme.Datum (Datum (..))
import Language.Scheme.Number (SchemeNumber (..))

render :: Datum -> Text
render = \case
    Bool True -> "#t"
    Bool False -> "#f"
    Number n -> renderNumber n
    Character c -> renderChar c
    String s -> renderString s
    Symbol s -> s
    Nil -> "()"
    Pair a b -> "(" <> renderPair a b <> ")"
    Vector ds -> "#(" <> Text.unwords (map render ds) <> ")"

renderPair :: Datum -> Datum -> Text
renderPair a b = case b of
    Nil -> render a
    Pair c d -> render a <> " " <> renderPair c d
    _ -> render a <> " . " <> render b

renderNumber :: SchemeNumber -> Text
renderNumber = \case
    ExactInteger n -> Text.pack (show n)
    ExactRational r -> renderExactRational r
    InexactReal d -> Text.pack (show d)
    ExactComplex re im -> renderExactRational re <> renderSign im <> renderExactRational (abs im) <> "i"
    InexactComplex re im -> Text.pack (show re) <> renderSign im <> Text.pack (show (abs im)) <> "i"
  where
    renderSign im = if im < 0 then "-" else "+"

renderExactRational :: Rational -> Text
renderExactRational r
    | denominator r == 1 = Text.pack (show (numerator r))
    | otherwise = Text.pack (show (numerator r)) <> "/" <> Text.pack (show (denominator r))

renderChar :: Char -> Text
renderChar = \case
    ' ' -> "#\\space"
    '\n' -> "#\\newline"
    c -> "#\\" <> Text.singleton c

renderString :: Text -> Text
renderString s = "\"" <> Text.concatMap escape s <> "\""
  where
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape '\n' = "\\n"
    escape '\t' = "\\t"
    escape '\r' = "\\r"
    escape '\a' = "\\a"
    escape '\b' = "\\b"
    escape c = Text.singleton c
