-- | Hedgehog generators mirroring the R5RS lexical grammar, used to drive
-- round-trip properties (@parse . render == Right@) against the reader.
module Language.Scheme.Gen
    ( genSymbol
    , genChar
    , genString
    , genNumber
    , genDatum
    ) where

import Data.Text (Text)
import Data.Text qualified as Text
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Language.Scheme.Datum (Datum (..), improperList, list)
import Language.Scheme.Number (SchemeNumber (..), mkComplex, mkRational)

genSymbol :: MonadGen m => m Text
genSymbol = Gen.choice [normalIdent, peculiar]
  where
    initials = ['a' .. 'z'] <> ['A' .. 'Z'] <> "!$%&*/:<=>?^_~"
    subsequents = initials <> ['0' .. '9'] <> "+-.@"
    normalIdent = do
        c <- Gen.element initials
        cs <- Gen.list (Range.linear 0 8) (Gen.element subsequents)
        pure (Text.pack (c : cs))
    peculiar = Gen.element ["+", "-", "..."]

-- | Any character R5RS can name (space, newline) plus printable ASCII,
-- since those are what our (bare-bones) named-character table and
-- @#\\<char>@ literal form actually round-trip.
genChar :: MonadGen m => m Char
genChar = Gen.choice [pure ' ', pure '\n', Gen.enum '!' '~']

-- | Printable ASCII plus every character our string escapes cover, so
-- the round-trip property actually exercises @renderString@\/@stringLit@
-- agreeing on each one.
genString :: MonadGen m => m Text
genString = Gen.text (Range.linear 0 12) (Gen.choice [pure ' ', pure '\n', pure '\t', pure '\r', pure '\a', pure '\b', Gen.enum '!' '~'])

genRealNumber :: MonadGen m => m SchemeNumber
genRealNumber =
    Gen.choice
        [ ExactInteger <$> Gen.integral (Range.linearFrom 0 (-1_000_000) 1_000_000)
        , mkRational <$> Gen.integral (Range.linearFrom 0 (-1_000) 1_000) <*> Gen.integral (Range.linear 1 1_000)
        , InexactReal <$> Gen.double (Range.linearFracFrom 0 (-1.0e6) 1.0e6)
        ]

genNumber :: MonadGen m => m SchemeNumber
genNumber =
    Gen.choice
        [ genRealNumber
        , mkComplex <$> genRealNumber <*> genRealNumber
        ]

genLeaf :: MonadGen m => m Datum
genLeaf =
    Gen.choice
        [ Symbol <$> genSymbol
        , Bool <$> Gen.bool
        , Number <$> genNumber
        , Character <$> genChar
        , String <$> genString
        ]

-- | Bounded-depth Scheme data: leaves, proper and dotted lists, and
-- vectors. 'Gen.small' shrinks the size budget at each level of nesting,
-- so 'Gen.recursive' always bottoms out at a leaf.
genDatum :: MonadGen m => m Datum
genDatum =
    Gen.recursive
        Gen.choice
        [genLeaf]
        [ Gen.small (list <$> Gen.list (Range.linear 0 4) genDatum)
        , Gen.small (improperList <$> Gen.list (Range.linear 1 4) genDatum <*> genLeaf)
        , Gen.small (Vector <$> Gen.list (Range.linear 0 4) genDatum)
        ]
