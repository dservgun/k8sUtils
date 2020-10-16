{-# language OverloadedStrings #-}
{-# language DerivingVia       #-}
{-# language InstanceSigs      #-}

-- @Author: dinkar
-- @Date:   2020-10-16 15:14:04
-- @Last Modified by:   dinkar
-- @Last Modified time: 2020-10-16 19:14:28

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
import Data.IP

newtype UserName = UserName Text
  deriving Show via Text
  deriving Read via Text

newtype Password = Password Text
  deriving Show via Text
  deriving Read via Text

data GlobalConfiguration = 
  GlobalConfiguration {
    _globalImageRegistry :: DockerImageRegistry
    , _globalImagePullSecrets :: DockerImagePullSecrets
    , _storageClass :: StorageClass
  }


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
    , _updateStrategy :: UpdateStrategy
    , _podAnnotations :: Set Annotation
    , _podLabels :: Set Label
    , _commonAnnotations :: Set Annotation
    , _commonLabels :: Set Label
    , _persistenceParameters :: PersistenceParameters
    , _podAffinity :: V1Affinity
    , _nodeSelector :: Set Label -- Node labels for pod assignment.
    , _tolerations :: Set V1Toleration
  }

data PersistenceParameters = PersistenceParameters {
  _persistenceEnabled :: Bool
  , _persistenceStorageClass :: V1StorageClass
  , _persistenceExistingClaim :: V1PersistentVolumeClaim
  , _persistenceAccessMode :: StorageAccessMode
  , _persistenceSize :: Quantity
}

data ServiceParameters = ServiceParameters {
  _serviceType :: CustomServiceType
  , _servicePort :: Port
  , _serviceNodePort :: Port
  , _serviceLoadBalancerIP :: IP
  , _serviceExternalTrafficPolicy :: ExternalTrafficPolicy
  , _serviceAnnotations :: Set Annotation
  , _serviceLoadBalancerSourceRanges :: Set LoadBalancerSourceRange
  , _serviceExtraPorts :: [Port]
  , _serviceHttpPort :: Port
}

data DiscourseParameters = DiscourseParameters {
  _discourseHost :: IP
  , _discourseSiteName :: SiteName
  , _discourseUserName :: UserName
  , _discoursePassword :: Password
  , _discourseExistingSecret :: V1Secret
  
}

