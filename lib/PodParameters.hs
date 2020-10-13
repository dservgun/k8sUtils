{-# language OverloadedStrings #-}
{-# language DerivingVia       #-}
{-# language RankNTypes        #-}

module PodParameters where

import CommonTypes
import Kubernetes.OpenAPI
import Lens.Micro

-- TODO: This is not as general as the pods specification.
-- We are probably using just as much convenience as we can get.
class PodParameters a where
  apiVersion :: Lens' a APIVersion
  metadataName :: Lens' a Name
  podName :: Lens' a PodName
  podImage :: Lens' a DockerImage
  restartPolicy :: Lens' a RestartPolicy

