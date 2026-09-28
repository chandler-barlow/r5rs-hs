-- | Lexical-level parsers for R5RS's external representation (report
-- section 7.1.1). This module knows what a token /is/ - where an
-- identifier ends, how a number literal is spelled, how a string escapes
-- its quotes - but nothing about how tokens nest into a datum. That's
-- 'Language.Scheme.Parser'.
--
-- Every exported parser consumes trailing whitespace and comments (a
-- 'lexeme'), so parsers built out of these never need to worry about
-- intertoken space themselves.
module Language.Scheme.Lexer
    ( Parser
    , sc
    , lexeme
    , parens
    , vecBrackets
    , boolLit
    , numberLit
    , charLit
    , stringLit
    , identifierLit
    , dotLit
    , quoteLit
    , quasiquoteLit
    , unquoteLit
    , unquoteSplicingLit
    ) where

import Control.Monad (guard, void)
import Data.Char (digitToInt, isAsciiLower, isAsciiUpper, isDigit)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Language.Scheme.Number (SchemeNumber (..), mkRational, toExact, toInexact)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

-- | Skip intertoken space: whitespace and @;@ line comments. R5RS has no
-- block comments (@#| |#@ and @#;@ are later-report additions).
sc :: Parser ()
sc = void $ many (void spaceChar <|> lineComment)
  where
    lineComment = char ';' *> void (takeWhileP (Just "comment") (/= '\n'))

lexeme :: Parser a -> Parser a
lexeme p = p <* sc

symbolText :: Text -> Parser Text
symbolText = lexeme . chunk

parens :: Parser a -> Parser a
parens = between (symbolText "(") (symbolText ")")

vecBrackets :: Parser a -> Parser a
vecBrackets = between (symbolText "#(") (symbolText ")")

quoteLit, quasiquoteLit, unquoteLit, unquoteSplicingLit, dotLit :: Parser ()
quoteLit = void $ symbolText "'"
quasiquoteLit = void $ symbolText "`"
unquoteSplicingLit = void $ try $ symbolText ",@"
unquoteLit = void $ lexeme $ char ',' <* notFollowedBy (char '@')
dotLit = void $ try $ lexeme $ char '.' <* notFollowedBy (satisfy isSubsequent)

boolLit :: Parser Bool
boolLit = lexeme $ try $ (True <$ chunk "#t") <|> (False <$ chunk "#f")

-- * Identifiers (report section 7.1.1)

isSpecialInitial :: Char -> Bool
isSpecialInitial c = c `elem` ("!$%&*/:<=>?^_~" :: String)

isAsciiLetter :: Char -> Bool
isAsciiLetter c = isAsciiLower c || isAsciiUpper c

isInitial :: Char -> Bool
isInitial c = isAsciiLetter c || isSpecialInitial c

isSpecialSubsequent :: Char -> Bool
isSpecialSubsequent c = c `elem` ("+-.@" :: String)

isSubsequent :: Char -> Bool
isSubsequent c = isInitial c || isDigit c || isSpecialSubsequent c

-- | An identifier, including the three peculiar identifiers @+@, @-@, and
-- @...@. Anything else starting with @+@\/@-@\/@.@ is a number (tried
-- first at the call site) or, failing that, a lexical error - R5RS does
-- not generalize peculiar identifiers to arbitrary @->foo@-style names.
identifierLit :: Parser Text
identifierLit = lexeme $ peculiar <|> normal
  where
    peculiar =
        try (string "..." <* notFollowedBy (satisfy isSubsequent))
            <|> try (Text.singleton <$> oneOf ("+-" :: String) <* notFollowedBy (satisfy isSubsequent))
    normal = do
        c <- satisfy isInitial
        cs <- many (satisfy isSubsequent)
        pure (Text.pack (c : cs))

-- * Characters (report section 7.1.1)

charLit :: Parser Char
charLit = lexeme $ try (chunk "#\\") *> namedOrLiteral
  where
    namedOrLiteral = try named <|> anySingle
    named = do
        c <- letterChar
        rest <- many (satisfy isSubsequent)
        if null rest
            then pure c
            else case c : rest of
                "space" -> pure ' '
                "newline" -> pure '\n'
                _ -> fail "unknown character name"

-- * Strings (report section 7.1.1)

stringLit :: Parser Text
stringLit = lexeme $ char '"' *> (Text.pack <$> many stringChar) <* char '"'
  where
    stringChar = escaped <|> satisfy (\c -> c /= '"' && c /= '\\')
    escaped = char '\\' *> (char '"' <|> char '\\')

-- * Numbers (report section 7.1.1)

data Radix = Bin | Oct | Dec | Hex

radixBase :: Radix -> Integer
radixBase = \case
    Bin -> 2
    Oct -> 8
    Dec -> 10
    Hex -> 16

data Exactness = Exact | Inexact

data Tag = TagRadix Radix | TagExact Exactness

numberLit :: Parser SchemeNumber
numberLit = lexeme $ try $ do
    tags <- many (try tag)
    let radix = lastOr Dec [r | TagRadix r <- tags]
        exactness = lastOr' [e | TagExact e <- tags]
    n <- signedReal radix
    notFollowedBy (satisfy isSubsequent)
    pure $ case exactness of
        Nothing -> n
        Just Exact -> toExact n
        Just Inexact -> toInexact n
  where
    lastOr d [] = d
    lastOr _ xs = last xs
    lastOr' [] = Nothing
    lastOr' xs = Just (last xs)
    tag =
        choice
            [ TagRadix Bin <$ chunk "#b"
            , TagRadix Oct <$ chunk "#o"
            , TagRadix Dec <$ chunk "#d"
            , TagRadix Hex <$ chunk "#x"
            , TagExact Exact <$ chunk "#e"
            , TagExact Inexact <$ chunk "#i"
            ]

signedReal :: Radix -> Parser SchemeNumber
signedReal radix = do
    neg <- option False (True <$ char '-' <|> False <$ char '+')
    n <- unsignedReal radix
    pure $ if neg then negateNumber n else n
  where
    negateNumber = \case
        ExactInteger i -> ExactInteger (negate i)
        ExactRational r -> ExactRational (negate r)
        InexactReal d -> InexactReal (negate d)

-- | Decimal points and exponents are only meaningful in radix 10; other
-- radixes admit only integers and ratios.
unsignedReal :: Radix -> Parser SchemeNumber
unsignedReal Dec = try decimal <|> ratioOrInteger Dec
unsignedReal radix = ratioOrInteger radix

ratioOrInteger :: Radix -> Parser SchemeNumber
ratioOrInteger radix = do
    n <- unsignedInteger radix
    denom <- optional (char '/' *> unsignedInteger radix)
    case denom of
        Nothing -> pure (ExactInteger n)
        Just 0 -> fail "zero denominator in rational literal"
        Just d -> pure (mkRational n d)

unsignedInteger :: Radix -> Parser Integer
unsignedInteger radix = foldl step 0 <$> some (digitFor radix)
  where
    step acc d = acc * radixBase radix + toInteger d

digitFor :: Radix -> Parser Int
digitFor = \case
    Bin -> digitToInt <$> oneOf ("01" :: String)
    Oct -> digitToInt <$> oneOf ("01234567" :: String)
    Dec -> digitToInt <$> digitChar
    Hex -> digitToInt <$> hexDigitChar

-- | @<uinteger10> . <uinteger10>? <suffix>?@ or @. <uinteger10> <suffix>?@
-- or @<uinteger10> <suffix>@, per report section 7.1.1. Always radix 10.
decimal :: Parser SchemeNumber
decimal = withPoint <|> withExponentOnly
  where
    withPoint = try $ do
        intPart <- many digitChar
        _ <- char '.'
        fracPart <- many digitChar
        guard (not (null intPart && null fracPart))
        expPart <- optional exponentPart
        pure (InexactReal (assembleDouble intPart fracPart expPart))
    withExponentOnly = do
        intPart <- some digitChar
        InexactReal . assembleDouble intPart "" . Just <$> exponentPart

exponentPart :: Parser Int
exponentPart = do
    _ <- oneOf ("eEsSfFdDlL" :: String)
    sign <- option "" (pure <$> oneOf ("+-" :: String))
    ds <- some digitChar
    pure (read (sign <> ds))

assembleDouble :: String -> String -> Maybe Int -> Double
assembleDouble intPart fracPart mExp =
    read (int' <> "." <> frac' <> expSuffix)
  where
    int' = if null intPart then "0" else intPart
    frac' = if null fracPart then "0" else fracPart
    expSuffix = maybe "" (\e -> "e" <> show e) mExp
