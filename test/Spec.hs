{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main (main) where

import qualified Property.BTDictProps as Props
import Test.Hspec
import qualified Unit.BTDictSpec as Unit

main = hspec $ do
  describe "Unit tests: BTDict" Unit.spec
  describe "Property tests: BTDict" Props.spec
