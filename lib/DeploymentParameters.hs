{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module DeploymentParameters where

import           CommonTypes
import           Data.Text
import           Kubernetes.OpenAPI
import           Lens.Micro

class DeploymentParameters a where
  {-# MINIMAL apiVersion, namespace, metadataName, appName, replicas, deploymentImage, containerPort, deploymentKind #-}
  apiVersion :: Lens' a APIVersion
  namespace :: Lens' a Namespace
  metadataName :: Lens' a Name
  appName :: Lens' a AppName
  replicas :: Lens' a ReplicaCount
  deploymentImage :: Lens' a DockerImage
  containerPort :: Lens' a ContainerPort
  deploymentKind :: Lens' a ConfigurationKind
