module Property.BTDictProps (spec) where

import qualified BTDict as D
import Data.List (sortOn)
import Data.Maybe (isJust)
import Test.Hspec
import Test.QuickCheck

neutral :: D.BTDict Int Int
neutral = mempty

fromListBT :: (Ord k) => [(k, v)] -> D.BTDict k v
fromListBT = foldl (\t (k, v) -> D.insertBT k v t) D.emptyBT

toAscListBT :: D.BTDict k v -> [(k, v)]
toAscListBT = D.foldrBT (:) []

eqByList :: (Eq k, Eq v) => D.BTDict k v -> D.BTDict k v -> Bool
eqByList a b = toAscListBT a == toAscListBT b

memberBT :: (Ord k) => k -> D.BTDict k v -> Bool
memberBT k d = isJust (D.lookupBT k d)

genPairs :: Gen [(Int, Int)]
genPairs = arbitrary

genDict :: Gen (D.BTDict Int Int)
genDict = fromListBT <$> genPairs

spec :: Spec
spec = do
  describe "Monoid laws (by map-equality)" $ do
    it "associativity: (a <> b) <> c == a <> (b <> c)" $
      property $
        forAll genDict $ \a ->
          forAll genDict $ \b ->
            forAll genDict $ \c ->
              eqByList ((a <> b) <> c) (a <> (b <> c))

    it "left identity: mempty <> a == a" $
      property $
        forAll genDict $ \a ->
          eqByList (neutral <> a) a

    it "right identity: a <> mempty == a" $
      property $
        forAll genDict $ \a ->
          eqByList (a <> neutral) a

  describe "Insert/Delete/Lookup properties" $ do
    it "lookup after insert returns the inserted value" $
      property $
        forAll genDict $ \d ->
          forAll (arbitrary :: Gen (Int, Int)) $ \(k, v) ->
            D.lookupBT k (D.insertBT k v d) === Just v

    it "key is not a member after delete" $
      property $
        forAll genDict $ \d ->
          forAll (arbitrary :: Gen Int) $ \k ->
            memberBT k (D.deleteBT k d) === False

  describe "Order & traversal invariants" $ do
    it "toAscListBT is sorted by ascending key" $
      property $
        forAll genDict $ \d ->
          let ks = map fst (toAscListBT d)
           in ks === sortOn id ks

    it "round-trip: fromListBT . toAscListBT == id (by map-equality)" $
      property $
        forAll genDict $ \d ->
          eqByList (fromListBT (toAscListBT d)) d

  describe "mapValues and filter" $ do
    it "mapValues (+1) increments all values, keys unchanged" $
      property $
        forAll genDict $ \d ->
          let d' = D.mapValuesBT (+ 1) d
              keysOld = map fst (toAscListBT d)
              keysNew = map fst (toAscListBT d')
              valsOld = map snd (toAscListBT d)
              valsNew = map snd (toAscListBT d')
           in conjoin
                [ keysNew === keysOld
                , valsNew === map (+ 1) valsOld
                ]

    it "filter keeps only even-key pairs" $
      property $
        forAll genDict $ \d ->
          let p (k, _v) = even k
              d' = D.filterBT p d
           in conjoin [property (p kv) | kv <- toAscListBT d']

  describe "Eq instance matches content equality" $ do
    it "d1 == d2  <=>  toAscListBT d1 == toAscListBT d2" $
      property $
        forAll genDict $ \a ->
          forAll genDict $ \b ->
            (a == b) === eqByList a b
