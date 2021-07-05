{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- @Author: dinkar
-- @Date:   2020-10-11 23:23:38
-- @Last Modified by:   dinkar
-- @Last Modified time: 2021-07-04 11:42:17
module CommonTypes where

import           Data.Default
import           Data.Text
import           GHC.Generics
import           Kubernetes.Client
import           Kubernetes.OpenAPI
import           Kubernetes.OpenAPI.CustomTypes
import           Lens.Micro
import           Numeric.Natural


newtype AppName = AppName Text
  deriving Show via Text
  deriving Read via Text
  deriving Generic

newtype PodName = PodName Text
  deriving Show via Text
  deriving Read via Text
  deriving Generic

newtype APIVersion = APIVersion Text
  deriving Show via Text
  deriving Read via Text
  deriving Generic

newtype ReplicaCount = ReplicaCount Natural
  deriving Show via Natural
  deriving Read via Natural
  deriving Generic

newtype DockerImage = DockerImage Text
  deriving Show via Text
  deriving Read via Text
  deriving Generic

newtype ContainerPort = ContainerPort Natural
  deriving Show via Natural
  deriving Read via Natural
  deriving Generic


data RestartPolicy = OnFailure | Never deriving (Show, Generic)

data GPUVendor = AMD | NVIDIA deriving (Generic)

data StorageAccessMode = RWO | ROX | RWX deriving (Show, Generic)

instance Show GPUVendor where
  show AMD    = "amd"
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
  deriving Generic

newtype Port =
  Port Natural
    deriving Generic
    deriving Show

newtype SiteName = SiteName Text
  deriving Show via Text
  deriving Read via Text
  deriving Generic

newtype SidecarContainer = SidecarContainer Text
  deriving Show via Text
  deriving Read via Text
  deriving Generic

newtype ServiceAccountName = ServiceAccountName Text
  deriving Show via Text
  deriving Read via Text
  deriving Generic

data UpdateStrategy = Recreate | RollingUpdate Int
  deriving (Show, Read, Generic)

newtype Annotation = Annotatoin Text
  deriving Show via Text
  deriving Read via Text
  deriving Generic

newtype Label = Label Text
  deriving Show via Text
  deriving Read via Text
  deriving Generic

-- TODO:Merge this with the V1StorageClass, somehow.
newtype StorageClass = StorageClass Text
  deriving Show via Text
  deriving Read via Text
  deriving Generic

newtype NameOverride = NameOverride Text
  deriving Show via Text
  deriving Read via Text
  deriving Generic

newtype Email = Email Text -- TODO: define the email type.
  deriving Show via Text
  deriving Read via Text
  deriving Generic

newtype Argument = Argument Text -- TODO : check if there is an argument type in the OpenAPI.
  deriving Show via Text
  deriving Read via Text
  deriving Generic

newtype CustomCommand = CustomCommand Text
  deriving Show via Text
  deriving Read via Text
  deriving Generic

newtype EnvVariable = EnvVariable Text
  deriving Show via Text
  deriving Read via Text
  deriving Generic

newtype EnvValue = EnvValue Text
  deriving Show via Text
  deriving Read via Text
  deriving Generic

newtype HostName = HostName Text
  deriving Show via Text
  deriving Read via Text
  deriving Generic

newtype EnvironmentVariable = EnvironmentVariable (EnvVariable, EnvValue)
  deriving (Show, Read)

newtype DNSLabelName = DNSLabelName Text
  deriving Show via Text
  deriving Read via Text
  deriving Generic

newtype UserName = UserName Text
  deriving Show via Text
  deriving Read via Text
  deriving Generic

newtype Password = Password Text
  deriving Show via Text
  deriving Read via Text
  deriving Generic

newtype DatabaseName = DatabaseName Text
  deriving Show via Text
  deriving Read via Text
  deriving Generic

newtype K8sKey = K8sKey Text
  deriving Show via Text
  deriving Read via Text
  deriving Generic

newtype HostPath = HostPath Text
  deriving Show via Text
  deriving Read via Text
  deriving Generic

newtype Host = Host (HostName, HostPath)
  deriving (Show, Read, Generic)

newtype ConfigurationKind = ConfigurationKind Text
  deriving Show via Text
  deriving Read via Text
  deriving Generic

{-|
  Guaranteed
  Burstable
  BestEffort
-}
newtype QOS = QOS Text
  deriving Show via Text
  deriving Read via Text
  deriving Generic
