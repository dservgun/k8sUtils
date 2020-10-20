{-# language OverloadedStrings #-}
{-# language DerivingVia       #-}
{-# language InstanceSigs      #-}

-- @Author: dinkar
-- @Date:   2020-10-16 15:14:04
-- @Last Modified by:   dinkar
-- @Last Modified time: 2020-10-19 21:30:42

{-|
  -- TODO : Add comments for each attribute.
  -- Using template haskell based lenses has issues
  -- generating source documentation.
-}

module Discourse.DiscourseConfiguration where

import Data.Set
import Data.Map
import Data.Text
import Numeric.Natural
import Kubernetes.OpenAPI
import Lens.Micro
import CommonTypes
import CommonConfiguration
import RedisParameters
import ProbeParameters
import SidekiqParameters
import PostgresParameters
import ExternalDBParameters
import ServiceParameters
import ImageTypes
import Data.IP

type DiscourseFullName = DNSLabelName

data GlobalConfiguration = 
  GlobalConfiguration {
    _globalImageRegistry :: DockerImageRegistry
    , _globalImagePullSecrets :: DockerImagePullSecrets
    , _storageClass :: StorageClass
  }


data DiscourseParameters = DiscourseParameters {
  _discourseHost :: IP
  , _discourseFullName :: DiscourseFullName
  , _discourseSiteName :: SiteName
  , _discourseUserName :: UserName
  , _discoursePassword :: Password
  , _discourseExistingSecret :: V1Secret
  , _discourseEmail :: Email
  , _discourseCommand :: CustomCommand -- Custom command to override the image command.
  , _discourseArgs :: [Argument]
  , _discourseSecurityContext :: V1SecurityContext
  , _discourseResources :: [V1ResourceRequirements]
  , _discourseLivenessProbe :: ProbeParameters
  , _discoruseReadinessProbe :: ProbeParameters
  , _discourseCustomLivenessProbe :: ProbeParameters
  , _discourseCustomReadinessProbe :: ProbeParameters
  , _discourseExtraEnvironmentVariables :: Set EnvironmentVariable
  , _discourseExtraEnvVarsConfigMaps :: V1ConfigMap
  , _discourseExtraVolumeMounts :: Set V1PersistentVolume
  , _discourseSkipInstall :: Bool
}


discourseHost :: Lens' DiscourseParameters IP
discourseHost =
  lens _discourseHost (\discourseParameters' host' -> discourseParameters' {_discourseHost = host'})

discourseFullName :: Lens' DiscourseParameters DiscourseFullName
discourseFullName =
  lens _discourseFullName (\discourseParameters' fullName' -> discourseParameters' {_discourseFullName = fullName'})