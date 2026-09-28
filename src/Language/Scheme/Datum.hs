-- | The result of reading R5RS's external representation: an s-expression
-- built from Scheme's actual data types (pairs, not a Haskell list),
-- exactly as R5RS chapter 7 specifies it. Evaluation is out of scope for
-- this reader; 'Datum' just represents data.
module Language.Scheme.Datum
    ( Datum (..)
    , list
    , improperList
    , properList
    ) where

import Data.Text (Text)
import Language.Scheme.Number (SchemeNumber)

data Datum
    = Symbol !Text
    | Bool !Bool
    | Number !SchemeNumber
    | Character !Char
    | String !Text
    | Pair Datum Datum
    | Nil
    | Vector [Datum]
    deriving stock (Eq, Show)

-- | A proper list, as cons cells terminated by 'Nil'.
list :: [Datum] -> Datum
list = foldr Pair Nil

-- | A list of elements followed by a dotted tail.
improperList :: [Datum] -> Datum -> Datum
improperList xs tl = foldr Pair tl xs

-- | The elements of a proper list, or 'Nothing' if the datum isn't one
-- (an atom, or a dotted/improper list).
properList :: Datum -> Maybe [Datum]
properList Nil = Just []
properList (Pair a b) = (a :) <$> properList b
properList _ = Nothing
