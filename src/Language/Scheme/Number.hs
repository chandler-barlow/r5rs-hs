-- | R5RS's numeric tower, restricted to the exact integer\/rational and
-- inexact real layers. Complex numbers are not part of this bare-bones
-- reader; a literal using @\@@ or a trailing @i@ suffix will simply fail to
-- parse.
module Language.Scheme.Number
    ( SchemeNumber (..)
    , mkRational
    , toExact
    , toInexact
    , numToDouble
    , numToRational
    , addNum
    , subNum
    , mulNum
    , divNum
    , compareNum
    ) where

import Data.Ratio (denominator, numerator, (%))

data SchemeNumber
    = ExactInteger !Integer
    | ExactRational !Rational
    | InexactReal !Double
    deriving stock (Eq, Show)

-- | Build a rational from a numerator and denominator, collapsing to
-- 'ExactInteger' when the result reduces to a whole number.
mkRational :: Integer -> Integer -> SchemeNumber
mkRational n d
    | denominator r == 1 = ExactInteger (numerator r)
    | otherwise = ExactRational r
  where
    r = n % d

toExact :: SchemeNumber -> SchemeNumber
toExact n@(ExactInteger _) = n
toExact n@(ExactRational _) = n
toExact (InexactReal d) = mkRational (numerator r) (denominator r)
  where
    r = toRational d

toInexact :: SchemeNumber -> SchemeNumber
toInexact (ExactInteger n) = InexactReal (fromInteger n)
toInexact (ExactRational r) = InexactReal (fromRational r)
toInexact n@(InexactReal _) = n

numToDouble :: SchemeNumber -> Double
numToDouble = \case
    ExactInteger n -> fromInteger n
    ExactRational r -> fromRational r
    InexactReal d -> d

-- | Only meaningful for exact numbers; callers must check 'InexactReal'
-- isn't present (arithmetic contagion rules do this already).
numToRational :: SchemeNumber -> Rational
numToRational = \case
    ExactInteger n -> n % 1
    ExactRational r -> r
    InexactReal d -> toRational d

isInexact :: SchemeNumber -> Bool
isInexact (InexactReal _) = True
isInexact _ = False

-- | Exact operands combine exactly (as 'Rational'); either operand being
-- inexact is contagious and forces a 'Double' result, per R5RS's
-- exactness rules.
exactBinOp :: (Rational -> Rational -> Rational) -> (Double -> Double -> Double) -> SchemeNumber -> SchemeNumber -> SchemeNumber
exactBinOp fR fD a b
    | isInexact a || isInexact b = InexactReal (fD (numToDouble a) (numToDouble b))
    | otherwise = let r = fR (numToRational a) (numToRational b) in mkRational (numerator r) (denominator r)

addNum, subNum, mulNum :: SchemeNumber -> SchemeNumber -> SchemeNumber
addNum = exactBinOp (+) (+)
subNum = exactBinOp (-) (-)
mulNum = exactBinOp (*) (*)

divNum :: SchemeNumber -> SchemeNumber -> Either String SchemeNumber
divNum a b
    | isInexact a || isInexact b = Right (InexactReal (numToDouble a / numToDouble b))
    | numToRational b == 0 = Left "division by zero"
    | otherwise = let r = numToRational a / numToRational b in Right (mkRational (numerator r) (denominator r))

compareNum :: SchemeNumber -> SchemeNumber -> Ordering
compareNum a b
    | isInexact a || isInexact b = compare (numToDouble a) (numToDouble b)
    | otherwise = compare (numToRational a) (numToRational b)
