import           Test.Tasty
import           Test.Tasty.SmallCheck as SC
import           Test.Tasty.QuickCheck as QC
import           Test.Tasty.HUnit
import           Test.QuickCheck.Modifiers

import           Data.Either        (isRight)
import           Data.List
import           Data.Ord
import qualified Data.Text          as T
import           Data.Text.IO       as T (readFile)

import Blockchain.Block
import Blockchain.Util

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps = testGroup "(checked by SmallCheck)"
  [ SC.testProperty "mCons Nothing to a list doesn't change it" $
      \list -> mCons Nothing (list :: [Int]) == list
  , SC.testProperty "mCons Just to a list prepends it" $
      \list x -> mCons (Just (x:: Int)) (list :: [Int]) == (x:list)
  ]

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "mList on non-empty list returns Just last" $
      \(NonEmpty list) -> mLast (list :: [Int]) == Just (Data.List.last list)
  ]

unitTests = testGroup "Unit tests"
  [ testCase "mLast on empty list returns Nothing" $
      mLast ([] :: [Int]) @?= Nothing
  , testCase "mLast on empty list returns Nothing" $ do
      blockLines <- T.lines <$> T.readFile "tests/block.chain"
      case validateTextChain blockLines of
        Left err -> assertFailure err
        Right () -> return ()
  ]
