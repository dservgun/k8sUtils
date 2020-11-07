{-# LANGUAGE OverloadedStrings #-}
-- @Author: dinkar
-- @Date:   2020-10-18 09:14:02
-- @Last Modified by:   dinkar
-- @Last Modified time: 2020-11-06 22:48:07

module CommonPredicates where

import           CommonTypes
import           Data.Coerce
import           Data.Functor.Contravariant
import           Data.Text                  as Text
import           Data.Text.Encoding         as Text
import           Numeric.Natural

-- DNS protocol limits the length of a label to 63 bytes
isValidDNSLabelName :: Predicate DNSLabelName
isValidDNSLabelName = Predicate (\a -> (Text.length $ coerce a) <= 63)


{- |
  * failureThreshold: When a probe fails, Kubernetes will try failureThreshold times before giving up.
  Giving up in case of liveness probe means restarting the container.
  In case of readiness probe the Pod will be marked Unready.
  Defaults to 3. Minimum value is 1.
-}
isValidFailureThreshold :: Predicate Natural
isValidFailureThreshold = Predicate (\a -> a >= 1 && a < 30)


{-|
  successThreshold: Minimum consecutive successes for the probe to be considered successful after having failed.
  Defaults to 1. Must be 1 for liveness. Minimum value is 1.
-}

isValidSuccessThreshold :: Predicate Natural
isValidSuccessThreshold = Predicate (\a -> a >= 1 && a < 30)


isValidConfigurationKind :: Predicate ConfigurationKind
isValidConfigurationKind =
  Predicate (\a -> validConfiguration . coerce $ a)
  where
    validConfiguration :: Text -> Bool
    validConfiguration a
      | a == "Deployment" = True
      | a == "Pod" = True
      | a == "PersistentVolumeClaim" = True
      | otherwise = False

{-|
  * Guaranteed
  * Burstable
  * BestEffort
-}
isValidQOS :: Predicate QOS
isValidQOS =
  Predicate $ validQOS . coerce
  where
    validQOS :: Text -> Bool
    validQOS qos
      | qos == "Guaranteed" = True
      | qos == "Burstable" = True
      | qos == "BestEffort" = True
      | otherwise = False
