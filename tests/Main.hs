module Main where

import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Control.Monad.MissingM
import Data.Maybe (listToMaybe)
import Data.Functor.Identity

main :: IO ()
main = defaultMain tests

tests :: [TF.Test]
tests = [
    testGroup "The" [
      testProperty "prop_findMvsfilter" prop_findMvsfilter,
      testProperty "prop_findMapMvsfilter" prop_findMapMvsfilter
    ]
  ]

instance Show (a -> b) where
  show = const "(function)"

-- just testing with the Identity monad for now.

prop_findMvsfilter :: (Int -> Bool) -> [Int] -> Bool
prop_findMvsfilter f as =
  let f' = return . f
  in runIdentity (findM f' as) == listToMaybe (filter f as)

prop_findMapMvsfilter :: (Int -> Int) -> (Int -> Bool) -> [Int] -> Bool
prop_findMapMvsfilter mapper tester as =
  let amb x = let b = mapper x in if tester b then Just b else Nothing
  in runIdentity (findMapM (return . amb) as) == (listToMaybe (filter tester (fmap mapper as)))
