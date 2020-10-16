{-# language OverloadedStrings #-}
{-# language DerivingVia       #-}
{-# language InstanceSigs      #-}

-- @Author: dinkar
-- @Date:   2020-10-16 15:14:04
-- @Last Modified by:   dinkar
-- @Last Modified time: 2020-10-16 16:28:06

{-|
  -- TODO : Add comments for each attribute.
  -- Using template haskell based lenses has issues
  -- generating source documentation.
-}

module Discourse.DiscourseConfiguration where

import Data.Set
import Data.Text
import Kubernetes.OpenAPI
import Lens.Micro
import CommonTypes
import ImageTypes


-- TODO:Merge this with the V1StorageClass, somehow.
newtype StorageClass = StorageClass Text
  deriving Show via Text
  deriving Read via Text

newtype NameOverride = NameOverride Text
  deriving Show via Text
  deriving Read via Text

data GlobalConfiguration = 
  GlobalConfiguration {
    _globalImageRegistry :: DockerImageRegistry
    , _globalImagePullSecrets :: DockerImagePullSecrets
    , _storageClass :: StorageClass
  }

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

data CommonConfiguration = 
  CommonConfiguration {
    _replicaCount :: ReplicaCount
    , _imageRegistry :: DockerImageRegistry
    , _imageRepository :: DockerImageRepository
    , _imageTag :: ImageTag
    , _imagePullPolicy :: ImagePullPolicy
    , _imageDebug :: Bool
    , _imagePullSecrets :: DockerImagePullSecrets
    , _nameOverride :: NameOverride
    , _fullNameOverride :: NameOverride
    , _extraVolumes :: [V1VolumeMount]
    , _sideCarContainers :: [SidecarContainer]
    , _initContainers :: [V1Container]
    , _serviceAccount :: ServiceAccountName
    , _createServiceAccount :: Bool
    , _podSecurityContext :: V1PodSecurityContext -- Use some sensible defaults.
    , _persistenceEnabled :: Bool -- Whether to enable persistence based on Persistent Volume Claims
    , _persistenceStorageClass :: V1StorageClass
    , _persistenceExistingClaim :: V1PersistentVolumeClaim
    , _persistenceAccessMode :: StorageAccessMode
    , _persistenceSize :: Quantity -- Default : Quantity "10Gi"
    , _updateStrategy :: UpdateStrategy
    , _podAnnotations :: Set Annotation
    , _podLabels :: Set Label
    , _commonAnnotations :: Set Annotation
    , _commonLabels :: Set Label
  }

