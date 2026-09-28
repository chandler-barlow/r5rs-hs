-- | Datum-level grammar (report section 7.1.2): how tokens from
-- 'Language.Scheme.Lexer' nest into lists, vectors, and quote
-- abbreviations.
module Language.Scheme.Parser
    ( Parser
    , ReadError
    , parseProgram
    , parseDatum
    , pDatum
    ) where

import Data.Text (Text)
import Data.Void (Void)
import Language.Scheme.Datum
import Language.Scheme.Lexer
import Text.Megaparsec

type ReadError = ParseErrorBundle Text Void

-- | Parse a whole program: zero or more top-level datums.
parseProgram :: FilePath -> Text -> Either ReadError [Datum]
parseProgram = parse (sc *> many pDatum <* eof)

-- | Parse a single datum, e.g. for a REPL that reads one form at a time.
parseDatum :: FilePath -> Text -> Either ReadError Datum
parseDatum = parse (sc *> pDatum <* eof)

pDatum :: Parser Datum
pDatum =
    choice
        [ Bool <$> boolLit
        , Number <$> numberLit
        , Character <$> charLit
        , String <$> stringLit
        , Symbol <$> identifierLit
        , pList
        , pVector
        , pAbbrev
        ]

pList :: Parser Datum
pList = parens $ do
    items <- many pDatum
    tailD <- option Nil (dotLit *> pDatum)
    pure (improperList items tailD)

pVector :: Parser Datum
pVector = Vector <$> vecBrackets (many pDatum)

pAbbrev :: Parser Datum
pAbbrev =
    choice
        [ wrap "unquote-splicing" <$> (unquoteSplicingLit *> pDatum)
        , wrap "quote" <$> (quoteLit *> pDatum)
        , wrap "quasiquote" <$> (quasiquoteLit *> pDatum)
        , wrap "unquote" <$> (unquoteLit *> pDatum)
        ]
  where
    wrap name d = list [Symbol name, d]
