{-# LANGUAGE ScopedTypeVariables #-}
{-# language OverloadedStrings   #-}

module TastyUnitTests where
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec
import Data.Functor.Contravariant
import Data.Text as Text
import CommonTypes
import CommonPredicates

spec_validDNSLabelName :: Spec
spec_validDNSLabelName = describe "ValidDNSName" $ do
  it "returns True" $ (getPredicate isValidDNSLabelName $ DNSLabelName "a") `shouldBe` True

spec_InvalidDNSLabelName :: Spec
spec_InvalidDNSLabelName =
  describe "Names longer than 63 characters is invalid" $ do
    it "returns False" $ (getPredicate isValidDNSLabelName $ DNSLabelName $ Text.replicate 64 "a") `shouldBe` False