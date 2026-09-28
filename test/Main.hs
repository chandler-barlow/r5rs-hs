module Main (main) where

import Hedgehog (checkParallel)
import Hedgehog.Main (defaultMain)
import Language.Scheme.EvalTest qualified as EvalTest
import Language.Scheme.LexerTest qualified as LexerTest
import Language.Scheme.ParserTest qualified as ParserTest

main :: IO ()
main =
    defaultMain
        [ checkParallel LexerTest.tests
        , checkParallel ParserTest.tests
        , checkParallel EvalTest.tests
        ]
