module Test ( tests ) where

import Distribution.TestSuite

--
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

spec :: Spec
spec = do
    describe "reverse test" $ do
        prop "int" (test_reverse :: [Int] -> Bool)
        prop "char" (test_reverse :: [Char] -> Bool)

-- リストの逆順の逆順は元と同じになる
test_reverse :: [a] -> Bool
test_reverse xs = xs == reverse (reverse xs)
--

tests :: IO [Test]
tests = return [ Test succeeds, Test fails, Test hoge ]
  where
    succeeds = TestInstance
        { run = return $ Finished Pass
        , name = "succeeds"
        , tags = []
        , options = []
        , setOption = \_ _ -> Right succeeds
        }
    fails = TestInstance
        { run = return $ Finished $ Fail "Always fails!"
        , name = "fails"
        , tags = []
        , options = []
        , setOption = \_ _ -> Right fails
        }
    hoge = TestInstance
        { run = return $ Finished Pass
        , name = "hoge"
        , tags = []
        , options = []
        , setOption = \_ _ -> Right hoge
        }
