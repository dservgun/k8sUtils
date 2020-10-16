{-# language OverloadedStrings #-}
{-# language DerivingVia       #-}
{-# language RankNTypes        #-}

-- @Author: dinkar
-- @Date:   2020-10-11 23:23:38
-- @Last Modified by:   dinkar
-- @Last Modified time: 2020-10-16 18:56:08
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
data StorageAccessMode = RWO | ROX | RWX deriving (Show)

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

data AffinityPreset = SoftAffinityPreset | HardAffinityPreset deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- TODO: These types are part of the k8s api and ideally need to be promoted upstream.
data CustomServiceType = ClusterIP | NodePort | LoadBalancer | ExternalName deriving (Show, Read, Eq, Ord, Enum, Bounded)
data ExternalTrafficPolicy = Local deriving (Show, Read, Eq, Ord, Enum, Bounded)
newtype LoadBalancerSourceRange = LoadBalancerSourceRange Text
  deriving Show via Text
  deriving Read via Text

type Port = IntOrString

newtype SiteName = SiteName Text
  deriving Show via Text
  deriving Read via Text

newtype SidecarContainer = SidecarContainer Text
  deriving Show via Text
  deriving Read via Text

newtype ServiceAccountName = ServiceAccountName Text
  deriving Show via Text
  deriving Read via Text

data UpdateStrategy = Recreate | RollingUpdate Int
  deriving (Show, Read)

newtype Annotation = Annotatoin Text
  deriving Show via Text
  deriving Read via Text

newtype Label = Label Text
  deriving Show via Text
  deriving Read via Text

-- TODO:Merge this with the V1StorageClass, somehow.
newtype StorageClass = StorageClass Text
  deriving Show via Text
  deriving Read via Text

newtype NameOverride = NameOverride Text
  deriving Show via Text
  deriving Read via Text
