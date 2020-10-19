{-# language OverloadedStrings #-}
{-# language DerivingVia       #-}
{-# language InstanceSigs      #-}

-- @Author: dinkar
-- @Date:   2020-10-16 15:14:04
-- @Last Modified by:   dinkar
-- @Last Modified time: 2020-10-18 20:54:24

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
import ImageTypes
import Data.IP

type DiscourseFullName = DNSLabelName

data GlobalConfiguration = 
  GlobalConfiguration {
    _globalImageRegistry :: DockerImageRegistry
    , _globalImagePullSecrets :: DockerImagePullSecrets
    , _storageClass :: StorageClass
  }


data IngressParameters = IngressParameters {
  _ingressEnabled :: Bool
  , _ingressCertifateManager :: Set Annotation -- TODO: What is the annotation here?
  , _ingressHostName :: HostName
  , _ingressTLS :: Bool
  , _ingressAnnotation :: Set Annotation
  , _ingressExtraHosts :: Map Host (Maybe V1Secret)
}

data PostgresParameters = PostgresParameters {
  _postgresEnabled :: Bool
  , _postgresqlUserName :: UserName
  , _postgressqlPassword :: Password
  , _postgressqlPostgresPassword :: Password
  , _postgresqlExistingSecret :: V1Secret
  , _postgresqlDatabaseName :: DatabaseName
  , _postgresqlPersistencEnabled :: Bool
  , _externalDBParameters :: ExternalDBParameters
}

data ExternalDBParameters = ExternalDBParameters {
  _hostName :: HostName
  , _portNumber :: Port
  , _externalUserName :: UserName
  , _externalPassword :: Password
  , _externalPostgresUser :: UserName
  , _externalPostgresPassword :: Password
  , _externalPostgresExistingSecret :: V1Secret
  , _externalDatabase :: DatabaseName
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