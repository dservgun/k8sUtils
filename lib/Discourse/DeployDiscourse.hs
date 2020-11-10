{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
-- @Author: dinkar
-- @Date:   2020-11-06 18:30:29
-- @Last Modified by:   dinkar
-- @Last Modified time: 2020-11-10 18:14:39

module Discourse.DeployDiscourse
  (
    makeDeployment
  ) where

import           CommonTypes
import           Data.Coerce
import           Data.Default
import           Data.Map                         as Map
import           Data.Text
import           DeploymentParameters
import           CommonConfiguration
import           Discourse.DiscourseConfiguration
import           GHC.Natural
import           Kubernetes.Client
import           Kubernetes.OpenAPI
import           Kubernetes.OpenAPI.API.AppsV1
import           Kubernetes.OpenAPI.API.CoreV1
import           Lens.Micro

makeDeployment :: DeploymentParameters a => a -> DiscourseParameters -> V1Deployment
makeDeployment deploymentParameters' discourseConfiguration' =
  mkV1Deployment &
    v1DeploymentApiVersionL .~ (Just . coerce $ deploymentParameters' ^. DeploymentParameters.apiVersion) &
    v1DeploymentKindL .~ (Just . coerce $ deploymentParameters' ^. DeploymentParameters.deploymentKind) &
    v1DeploymentMetadataL .~ (Just mkV1ObjectMeta) &
    v1DeploymentMetadataL . _Just . v1ObjectMetaNameL .~ (Just . coerce $ discourseConfiguration' ^. discourseFullName) &
    v1DeploymentMetadataL . _Just . v1ObjectMetaLabelsL .~ (Just $ makeLabelMap discourseConfiguration') &
    v1DeploymentSpecL .~ (Just $ mkV1DeploymentSpec mkV1LabelSelector mkV1PodTemplateSpec) &
    v1DeploymentSpecL . _Just . v1DeploymentSpecReplicasL .~ 
      (Just . naturalToInt . coerce $ discourseConfiguration' ^. discourseInheritedParameters ^. replicaCount)




