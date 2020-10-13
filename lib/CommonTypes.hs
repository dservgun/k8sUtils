{-# language OverloadedStrings #-}
{-# language DerivingVia       #-}
{-# language RankNTypes        #-}

-- @Author: dinkar
-- @Date:   2020-10-11 23:23:38
-- @Last Modified by:   dinkar
-- @Last Modified time: 2020-10-12 22:21:49
module CommonTypes where

import Data.Text
import Data.Default
import Numeric.Natural
import Kubernetes.Client
import Kubernetes.OpenAPI
import Lens.Micro

newtype AppName = AppName Text
  deriving Show via Text
  deriving Read via Text

newtype PodName = PodName Text
  deriving Show via Text
  deriving Read via Text

newtype APIVersion = APIVersion Text
  deriving Show via Text
  deriving Read via Text

newtype ReplicaCount = ReplicaCount Natural
  deriving Show via Natural
  deriving Read via Natural

newtype DockerImage = DockerImage Text
  deriving Show via Text
  deriving Read via Text

newtype ContainerPort = ContainerPort Natural
  deriving Show via Natural 
  deriving Read via Natural

data RestartPolicy = OnFailure | Never deriving (Show)
data GPUVendor = AMD | NVIDIA

instance Show GPUVendor where
  show AMD = "amd"
  show NVIDIA = "nvidia"


instance Default APIVersion where
  def = APIVersion "apps/v1"

instance Default GPUVendor where
  def = NVIDIA

instance Default RestartPolicy where
  def = OnFailure -- TODO: Is this really the default.

instance Default Namespace where
  def = Namespace "default"
