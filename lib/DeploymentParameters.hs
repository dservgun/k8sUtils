{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module DeploymentParameters where

import           CommonTypes
import           Kubernetes.OpenAPI
import           Lens.Micro

class DeploymentParameters a where
  apiVersion :: Lens' a APIVersion
  namespace :: Lens' a Namespace
  metadataName :: Lens' a Name
  appName :: Lens' a AppName
  replicas :: Lens' a ReplicaCount
  deploymentImage :: Lens' a DockerImage
  containerPort :: Lens' a ContainerPort
