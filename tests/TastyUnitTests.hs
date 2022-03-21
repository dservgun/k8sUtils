{-# LANGUAGE ScopedTypeVariables #-}
{-# language OverloadedStrings   #-}

module TastyUnitTests where
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec
import Data.Functor.Contravariant
import Data.Coerce
import Data.Text as Text
import CommonTypes
import CommonPredicates
import ImageTypes

spec_validDNSLabelName :: Spec
spec_validDNSLabelName = describe "ValidDNSName" $ do
  it "returns True" $ (getPredicate isValidDNSLabelName $ DNSLabelName "a") `shouldBe` True

spec_InvalidDNSLabelName :: Spec
spec_InvalidDNSLabelName =
  describe "Names longer than 63 characters is invalid" $ do
    it "returns False" $ (getPredicate isValidDNSLabelName $ DNSLabelName $ Text.replicate 64 "a") `shouldBe` False

spec_isValidFailureThreshold :: Spec
spec_isValidFailureThreshold =
  describe "Failure threshold should pass" $ do
      it "returns true" $ (getPredicate isValidFailureThreshold $ 1) `shouldBe` True

spec_isValidSuccessThreshold :: Spec
spec_isValidSuccessThreshold =
  describe "Success threshold test should pass" $ do
    it "returns true" $ (getPredicate isValidSuccessThreshold 1) `shouldBe` True


spec_isValidConfigurationKind :: Spec
spec_isValidConfigurationKind =
  describe "Valid configuration kind" $ do
      it "returns Deployment" $ (getPredicate isValidConfigurationKind (ConfigurationKind "Deployment")
        ) `shouldBe` True

spec_isValidQOS :: Spec
spec_isValidQOS =
  describe "Valid QOS" $ do
    it "returns Guaranteed" $ (getPredicate isValidQOS (QOS "Guaranteed")) `shouldBe` True

