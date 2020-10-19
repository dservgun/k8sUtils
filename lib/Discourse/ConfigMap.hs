-- @Author: dinkar
-- @Date:   2020-10-17 18:46:10
-- @Last Modified by:   dinkar
-- @Last Modified time: 2020-10-19 16:54:45
{-# language OverloadedStrings #-}
{-# language InstanceSigs      #-}

module Discourse.ConfigMap where

import CommonTypes
import PodParameters
import Data.Coerce
import Data.Default
import Data.Map
import Data.Text as Text
import Kubernetes.Client
import Kubernetes.OpenAPI
import Kubernetes.OpenAPI.API.AppsV1
import Kubernetes.OpenAPI.API.CoreV1
import Lens.Micro
import GHC.Natural
import Discourse.DiscourseConfiguration

makeConfig :: DiscourseParameters -> V1ConfigMap
makeConfig discourseParameters =
  mkV1ConfigMap &
    v1ConfigMapApiVersionL .~ (Just "v1") &
    v1ConfigMapKindL .~ (Just "ConfigMap") &
    v1ConfigMapMetadataL .~ (Just mkV1ObjectMeta) &
    v1ConfigMapMetadataL . _Just . v1ObjectMetaNameL .~ (Just . coerce $ discourseParameters ^. discourseFullName)
