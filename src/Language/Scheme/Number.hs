-- | R5RS's numeric tower: exact integers and rationals, inexact reals,
-- and (new) complex numbers in rectangular form only - no polar syntax
-- (@a\@b@) and no transcendental functions (@sqrt@, @sin@, ...; those
-- would need a lot more machinery than the tower itself and aren't
-- implemented at all yet). A complex number's real and imaginary parts
-- always share one exactness: if either part written in a literal is
-- inexact, the whole number is inexact.
--
-- Ordering (@compareNum@) is only defined for reals, matching R5RS
-- (complex numbers aren't ordered) - callers must check 'isRealNum'
-- first; 'compareNum' and 'numToRational'\/'numToDouble' error out on a
-- complex argument rather than silently doing something wrong.
-- Numerical equality ('numEqual'), which R5RS does define across the
-- whole tower, does not have this restriction.
module Language.Scheme.Number
    ( SchemeNumber (..)
    , mkRational
    , mkComplex
    , negateNum
    , toExact
    , toInexact
    , numToDouble
    , numToRational
    , addNum
    , subNum
    , mulNum
    , divNum
    , compareNum
    , numEqual
    , isComplexNum
    , isRealNum
    , isRationalNum
    , isIntegerNum
    , isExactNum
    , realPartOf
    , imagPartOf
    ) where

import Data.Bifunctor (bimap)
import Data.Ratio (denominator, numerator, (%))

data SchemeNumber
    = ExactInteger !Integer
    | ExactRational !Rational
    | InexactReal !Double
    | -- | Real and imaginary parts, both exact. Never holds a zero
      -- imaginary part - 'mkExactComplex' collapses that to a real.
      ExactComplex !Rational !Rational
    | -- | As 'ExactComplex', but inexact; likewise never a zero
      -- imaginary part.
      InexactComplex !Double !Double
    deriving stock (Eq, Show)

-- | Build a rational from a numerator and denominator, collapsing to
-- 'ExactInteger' when the result reduces to a whole number.
mkRational :: Integer -> Integer -> SchemeNumber
mkRational n d
    | denominator r == 1 = ExactInteger (numerator r)
    | otherwise = ExactRational r
  where
    r = n % d

mkExactComplex :: Rational -> Rational -> SchemeNumber
mkExactComplex re im
    | im == 0 = mkRational (numerator re) (denominator re)
    | otherwise = ExactComplex re im

mkInexactComplex :: Double -> Double -> SchemeNumber
mkInexactComplex re im
    | im == 0 = InexactReal re
    | otherwise = InexactComplex re im

-- | Combine two real numbers into a (possibly-real, if the imaginary
-- part is zero) complex number, promoting both to inexact if either
-- input is. This is what a complex literal like @3+4i@ or @1.0+2i@
-- builds from its two real components.
mkComplex :: SchemeNumber -> SchemeNumber -> SchemeNumber
mkComplex re im
    | isInexactReal re || isInexactReal im = mkInexactComplex (numToDouble re) (numToDouble im)
    | otherwise = mkExactComplex (numToRational re) (numToRational im)
  where
    isInexactReal (InexactReal _) = True
    isInexactReal _ = False

negateNum :: SchemeNumber -> SchemeNumber
negateNum = \case
    ExactInteger n -> ExactInteger (negate n)
    ExactRational r -> ExactRational (negate r)
    InexactReal d -> InexactReal (negate d)
    ExactComplex re im -> ExactComplex (negate re) (negate im)
    InexactComplex re im -> InexactComplex (negate re) (negate im)

toExact :: SchemeNumber -> SchemeNumber
toExact n@(ExactInteger _) = n
toExact n@(ExactRational _) = n
toExact (InexactReal d) = ratToExact d
toExact n@(ExactComplex _ _) = n
toExact (InexactComplex re im) = mkExactComplex (toRational re) (toRational im)

ratToExact :: Double -> SchemeNumber
ratToExact d = mkRational (numerator r) (denominator r)
  where
    r = toRational d

toInexact :: SchemeNumber -> SchemeNumber
toInexact (ExactInteger n) = InexactReal (fromInteger n)
toInexact (ExactRational r) = InexactReal (fromRational r)
toInexact n@(InexactReal _) = n
toInexact (ExactComplex re im) = mkInexactComplex (fromRational re) (fromRational im)
toInexact n@(InexactComplex _ _) = n

-- | Real numbers only - see the module haddock.
numToDouble :: SchemeNumber -> Double
numToDouble = \case
    ExactInteger n -> fromInteger n
    ExactRational r -> fromRational r
    InexactReal d -> d
    n -> error ("numToDouble: not a real number: " <> show n)

-- | Real numbers only - see the module haddock.
numToRational :: SchemeNumber -> Rational
numToRational = \case
    ExactInteger n -> n % 1
    ExactRational r -> r
    InexactReal d -> toRational d
    n -> error ("numToRational: not a real number: " <> show n)

isComplexNum :: SchemeNumber -> Bool
isComplexNum (ExactComplex _ _) = True
isComplexNum (InexactComplex _ _) = True
isComplexNum _ = False

isRealNum :: SchemeNumber -> Bool
isRealNum = not . isComplexNum

-- | True of any real that isn't NaN\/infinite; we don't produce those
-- from literals, but a host-supplied 'InexactReal' could carry one.
isRationalNum :: SchemeNumber -> Bool
isRationalNum = \case
    ExactInteger _ -> True
    ExactRational _ -> True
    InexactReal d -> not (isNaN d || isInfinite d)
    _ -> False

isIntegerNum :: SchemeNumber -> Bool
isIntegerNum = \case
    ExactInteger _ -> True
    InexactReal d -> not (isNaN d || isInfinite d) && d == fromInteger (round d :: Integer)
    _ -> False

isExactNum :: SchemeNumber -> Bool
isExactNum = \case
    InexactReal _ -> False
    InexactComplex _ _ -> False
    _ -> True

-- | The real component of a real or complex number (a real number's own
-- value, unchanged).
realPartOf :: SchemeNumber -> SchemeNumber
realPartOf = \case
    ExactComplex re _ -> mkRational (numerator re) (denominator re)
    InexactComplex re _ -> InexactReal re
    n -> n

-- | The imaginary component: exact\/inexact zero for a real number.
imagPartOf :: SchemeNumber -> SchemeNumber
imagPartOf = \case
    ExactComplex _ im -> mkRational (numerator im) (denominator im)
    InexactComplex _ im -> InexactReal im
    InexactReal _ -> InexactReal 0
    _ -> ExactInteger 0

-- | A real or complex number's parts, as a common representation for
-- arithmetic: @Left@ when both parts are exact (share a 'Rational'
-- representation), @Right@ when either is inexact (share a 'Double'
-- one). A real number's imaginary part is 0.
asParts :: SchemeNumber -> Either (Rational, Rational) (Double, Double)
asParts = \case
    ExactInteger n -> Left (n % 1, 0)
    ExactRational r -> Left (r, 0)
    InexactReal d -> Right (d, 0)
    ExactComplex re im -> Left (re, im)
    InexactComplex re im -> Right (re, im)

-- | Apply a binary operation across the whole tower: real operands stay
-- exact when both are, complex arithmetic falls out of the same
-- formula (a real is just a complex number with a zero imaginary part),
-- and either operand being inexact is contagious.
complexBinOp ::
    (Rational -> Rational -> Rational -> Rational -> (Rational, Rational)) ->
    (Double -> Double -> Double -> Double -> (Double, Double)) ->
    SchemeNumber ->
    SchemeNumber ->
    SchemeNumber
complexBinOp fR fD a b = case (asParts a, asParts b) of
    (Left (ar, ai), Left (br, bi)) -> uncurry mkExactComplex (fR ar ai br bi)
    (pa, pb) -> uncurry mkInexactComplex (fD ar ai br bi)
      where
        (ar, ai) = toD pa
        (br, bi) = toD pb
        toD = either (bimap fromRational fromRational) id

addNum, subNum, mulNum :: SchemeNumber -> SchemeNumber -> SchemeNumber
addNum = complexBinOp (\ar ai br bi -> (ar + br, ai + bi)) (\ar ai br bi -> (ar + br, ai + bi))
subNum = complexBinOp (\ar ai br bi -> (ar - br, ai - bi)) (\ar ai br bi -> (ar - br, ai - bi))
mulNum =
    complexBinOp
        (\ar ai br bi -> (ar * br - ai * bi, ar * bi + ai * br))
        (\ar ai br bi -> (ar * br - ai * bi, ar * bi + ai * br))

divNum :: SchemeNumber -> SchemeNumber -> Either String SchemeNumber
divNum a b = case (asParts a, asParts b) of
    (Left (ar, ai), Left (br, bi))
        | br * br + bi * bi == 0 -> Left "division by zero"
        | otherwise -> Right (uncurry mkExactComplex (divR ar ai br bi))
    (pa, pb) -> Right (uncurry mkInexactComplex (divD ar ai br bi))
      where
        (ar, ai) = toD pa
        (br, bi) = toD pb
        toD = either (bimap fromRational fromRational) id
  where
    divR ar ai br bi = ((ar * br + ai * bi) / denom, (ai * br - ar * bi) / denom)
      where
        denom = br * br + bi * bi
    divD ar ai br bi = ((ar * br + ai * bi) / denom, (ai * br - ar * bi) / denom)
      where
        denom = br * br + bi * bi

-- | Real numbers only - see the module haddock.
compareNum :: SchemeNumber -> SchemeNumber -> Ordering
compareNum a b
    | isComplexNum a || isComplexNum b = error "compareNum: complex numbers are not ordered"
    | isInexact a || isInexact b = compare (numToDouble a) (numToDouble b)
    | otherwise = compare (numToRational a) (numToRational b)
  where
    isInexact (InexactReal _) = True
    isInexact _ = False

-- | Numerical equality across the whole tower, including complex
-- numbers - unlike 'compareNum', which only orders reals.
numEqual :: SchemeNumber -> SchemeNumber -> Bool
numEqual a b = case (asParts a, asParts b) of
    (Left (ar, ai), Left (br, bi)) -> ar == br && ai == bi
    (pa, pb) -> toD pa == toD pb
  where
    toD = either (bimap fromRational fromRational) id
