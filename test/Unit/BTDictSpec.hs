module Unit.BTDictSpec (spec) where

import qualified BTDict as D
import Test.Hspec

fromListBT :: (Ord k) => [(k, v)] -> D.BTDict k v
fromListBT = foldl (\t (k, v) -> D.insertBT k v t) D.emptyBT

toAscListBT :: D.BTDict k v -> [(k, v)]
toAscListBT = D.foldrBT (:) []

keys :: D.BTDict k v -> [k]
keys = map fst . toAscListBT

elems :: D.BTDict k v -> [v]
elems = map snd . toAscListBT

spec :: Spec
spec = do
  describe "insertBT / lookupBT" $ do
    it "finds inserted keys and returns correct values" $ do
      let d :: D.BTDict Int String
          d = fromListBT [(3, "c"), (1, "a"), (2, "b")]
      D.lookupBT (1 :: Int) d `shouldBe` Just "a"
      D.lookupBT (2 :: Int) d `shouldBe` Just "b"
      D.lookupBT (3 :: Int) d `shouldBe` Just "c"
      D.lookupBT (4 :: Int) d `shouldBe` Nothing

    it "updates value when inserting the same key" $ do
      let d1 :: D.BTDict Int String
          d1 = fromListBT [(1, "a"), (2, "b")]
          d2 = D.insertBT (2 :: Int) "B" d1
      D.lookupBT (2 :: Int) d2 `shouldBe` Just "B"

  describe "deleteBT" $ do
    it "deletes a leaf" $ do
      let d :: D.BTDict Int String
          d = fromListBT [(1, "a"), (2, "b"), (3, "c")]
          d' = D.deleteBT (3 :: Int) d
      D.lookupBT (3 :: Int) d' `shouldBe` Nothing
      keys d' `shouldBe` ([1, 2] :: [Int])

    it "deletes a node with one child" $ do
      let d :: D.BTDict Int String
          d = fromListBT [(2, "b"), (1, "a")]
          d' = D.deleteBT (2 :: Int) d
      D.lookupBT (2 :: Int) d' `shouldBe` Nothing
      keys d' `shouldBe` ([1] :: [Int])

    it "deletes a node with two children (using min of right)" $ do
      let d :: D.BTDict Int String
          d = fromListBT [(5, "e"), (3, "c"), (7, "g"), (6, "f"), (8, "h"), (4, "d")]
          d' = D.deleteBT (5 :: Int) d
      D.lookupBT (5 :: Int) d' `shouldBe` Nothing
      keys d' `shouldBe` ([3, 4, 6, 7, 8] :: [Int])

  describe "filterBT" $ do
    it "keeps only pairs satisfying predicate" $ do
      let d :: D.BTDict Int String
          d = fromListBT [(1, "a"), (2, "bb"), (3, "ccc"), (4, "dd")]
          p (k, _v) = even k
          d' = D.filterBT p d
      keys d' `shouldBe` ([2, 4] :: [Int])
      elems d' `shouldBe` (["bb", "dd"] :: [String])

  describe "mapValuesBT" $ do
    it "maps over values, keys intact" $ do
      let d :: D.BTDict Int Int
          d = fromListBT [(1, 10), (2, 20), (3, 30)]
          d' = D.mapValuesBT (+ 1) d
      keys d' `shouldBe` ([1, 2, 3] :: [Int])
      elems d' `shouldBe` ([11, 21, 31] :: [Int])

    describe "folds" $ do
      it "foldlBT sums values in-order" $ do
        let d :: D.BTDict Int Int
            d = fromListBT [(1, 1), (2, 2), (3, 3)]
        D.foldlBT (\acc (_k, v) -> acc + v) (0 :: Int) d `shouldBe` 6

    it "foldrBT builds an ascending list of pairs" $ do
      let d :: D.BTDict Int Char
          d = fromListBT [(1, 'a'), (2, 'b'), (3, 'c')]
          asc :: [(Int, Char)]
          asc = D.foldrBT (:) [] d
      asc `shouldBe` [(1, 'a'), (2, 'b'), (3, 'c')]
      asc `shouldBe` toAscListBT d

  describe "Semigroup/Monoid" $ do
    it "<> is right-biased on conflicting keys" $ do
      let a :: D.BTDict Int String
          a = fromListBT [(1, "a"), (2, "x")]
          b :: D.BTDict Int String
          b = fromListBT [(2, "b"), (3, "c")]
          c = a <> b
      toAscListBT c `shouldBe` [(1, "a"), (2, "b"), (3, "c")]

    it "mempty is identity" $ do
      let d :: D.BTDict Int Int
          d = fromListBT [(1, 10), (2, 20)]
      toAscListBT d `shouldBe` toAscListBT d
      toAscListBT d `shouldBe` toAscListBT d

  describe "Eq ignores tree shape (same mapping)" $ do
    it "different insertion orders yield equal dicts" $ do
      let a :: D.BTDict Int Char
          a = fromListBT [(2, 'b'), (1, 'a'), (3, 'c')]
          b :: D.BTDict Int Char
          b = fromListBT [(1, 'a'), (3, 'c'), (2, 'b')]
      a == b `shouldBe` True
