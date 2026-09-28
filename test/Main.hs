module Main (main) where

import Hedgehog (checkParallel)
import Hedgehog.Main (defaultMain)
import Language.Scheme.CallCCTest qualified as CallCCTest
import Language.Scheme.EvalTest qualified as EvalTest
import Language.Scheme.LexerTest qualified as LexerTest
import Language.Scheme.MacroTest qualified as MacroTest
import Language.Scheme.NumberTowerTest qualified as NumberTowerTest
import Language.Scheme.ParserTest qualified as ParserTest

main :: IO ()
main =
    defaultMain
        [ checkParallel LexerTest.tests
        , checkParallel ParserTest.tests
        , checkParallel EvalTest.tests
        , checkParallel MacroTest.tests
        , checkParallel CallCCTest.tests
        , checkParallel NumberTowerTest.tests
        ]
