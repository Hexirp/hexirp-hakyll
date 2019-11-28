--------------------------------------------------------------------------------
module Hexyll.Core.Util.StringTest
    ( test_tests
    ) where


--------------------------------------------------------------------------------
import           Test.Tasty              (TestTree, testGroup)
import           Test.Tasty.HUnit        ((@=?))


--------------------------------------------------------------------------------
import           Hexyll.Core.Util.String
import           TestSuite.Util


--------------------------------------------------------------------------------
test_tests :: TestTree
test_tests = testGroup "Hexyll.Core.Util.StringTest" $ concat
    [ fromAssertions "trim"
        [ "foo" @=? trim " foo\n\t "
        ]

    , fromAssertions "replaceAll"
        [ "32 & 131" @=? replaceAll "0x[0-9]+" (show . readInt) "0x20 & 0x83"
        ]

    , fromAssertions "splitAll"
        [ ["λ", "∀x.x", "hi"] @=? splitAll ", *" "λ, ∀x.x,  hi"
        ]

    , fromAssertions "needlePrefix"
        [ Just "ab" @=? needlePrefix "cd" "abcde"
        , Just "xx" @=? needlePrefix "ab" "xxab"
        , Nothing   @=? needlePrefix "a" "xx"
        , Just "x"  @=? needlePrefix "ab" "xabxab"
        , Just ""   @=? needlePrefix "ab" "abc"
        , Just ""   @=? needlePrefix "ab" "abab"
        , Nothing   @=? needlePrefix "" ""
        ]
    ]

  where
    readInt :: String -> Int
    readInt = read
