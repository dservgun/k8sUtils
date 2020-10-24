{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

-- @Author: dinkar
-- @Date:   2020-10-16 15:14:04
-- @Last Modified by:   dinkar
-- @Last Modified time: 2020-10-22 23:32:22

{-|
  -- TODO : Add comments for each attribute.
  -- Using template haskell based lenses has issues
  -- generating source documentation.
-}

module Discourse.DiscourseConfiguration where

import           CommonConfiguration
import           CommonTypes
import           Data.IP
import           Data.Map
import           Data.Set
import           Data.Text
import           ExternalDBParameters
import           ImageTypes
import           Kubernetes.OpenAPI
import           Lens.Micro
import           Numeric.Natural
import           PostgresParameters
import           ProbeParameters
import           RedisParameters
import           ServiceParameters
import           SidekiqParameters

type DiscourseFullName = DNSLabelName

data GlobalConfiguration =
  GlobalConfiguration {
    _globalImageRegistry      :: DockerImageRegistry
    , _globalImagePullSecrets :: DockerImagePullSecrets
    , _storageClass           :: StorageClass
  }


data DiscourseParameters = DiscourseParameters {
  _discourseHost                        :: IP
  , _discourseFullName                  :: DiscourseFullName
  , _discourseSiteName                  :: SiteName
  , _discourseUserName                  :: UserName
  , _discoursePassword                  :: Password
  , _discourseExistingSecret            :: V1Secret
  , _discourseEmail                     :: Email
  , _discourseCommand                   :: CustomCommand -- Custom command to override the image command.
  , _discourseArgs                      :: [Argument]
  , _discourseSecurityContext           :: V1SecurityContext
  , _discourseResources                 :: [V1ResourceRequirements]
  , _discourseLivenessProbe             :: ProbeParameters
  , _discourseReadinessProbe            :: ProbeParameters
  , _discourseCustomLivenessProbe       :: ProbeParameters
  , _discourseCustomReadinessProbe      :: ProbeParameters
  , _discourseExtraEnvironmentVariables :: Set EnvironmentVariable
  , _discourseExtraEnvVarsConfigMap     :: V1ConfigMap
  , _discourseExtraVolumeMounts         :: Set V1PersistentVolume
  , _discourseSkipInstall               :: Bool
}


discourseHost :: Lens' DiscourseParameters IP
discourseHost =
  lens _discourseHost (\discourseParameters' host' -> discourseParameters' {_discourseHost = host'})

discourseFullName :: Lens' DiscourseParameters DiscourseFullName
discourseFullName =
  lens _discourseFullName (\discourseParameters' fullName' -> discourseParameters' {_discourseFullName = fullName'})

discourseSiteName :: Lens' DiscourseParameters SiteName
discourseSiteName =
  lens _discourseSiteName (\discourseParameters' siteName' -> discourseParameters' {_discourseSiteName = siteName'})

discourseUserName :: Lens' DiscourseParameters UserName
discourseUserName =
  lens _discourseUserName (\discourseParameters' userName' -> discourseParameters' {_discourseUserName = userName'})

discoursePassword :: Lens' DiscourseParameters Password
discoursePassword =
  lens _discoursePassword (\discourseParameters' password' -> discourseParameters' {_discoursePassword = password'})

discourseExistingSecret :: Lens' DiscourseParameters V1Secret
discourseExistingSecret =
  lens _discourseExistingSecret (\discourseParameters' secret' -> discourseParameters' {_discourseExistingSecret = secret'})

discourseEmail :: Lens' DiscourseParameters Email
discourseEmail =
  lens _discourseEmail (\discourseParameters' email' -> discourseParameters' {_discourseEmail = email'})

discourseCommand :: Lens' DiscourseParameters CustomCommand
discourseCommand =
  lens _discourseCommand (\discourseParameters' command' -> discourseParameters' {_discourseCommand = command'})

discourseArgs :: Lens' DiscourseParameters [Argument]
discourseArgs =
  lens _discourseArgs (\discourseParameters' args' -> discourseParameters' {_discourseArgs = args'})

discourseSecurityContext :: Lens' DiscourseParameters V1SecurityContext
discourseSecurityContext =
  lens _discourseSecurityContext (\discourseParameters' secContext' -> discourseParameters' {_discourseSecurityContext = secContext'})

discourseResources :: Lens' DiscourseParameters [V1ResourceRequirements]
discourseResources =
  lens _discourseResources (\discourseParameters' resources' -> discourseParameters' {_discourseResources = resources'})

discourseLivenessProbe :: Lens' DiscourseParameters ProbeParameters
discourseLivenessProbe =
  lens _discourseLivenessProbe (\discourseParameters' probe' -> discourseParameters' {_discourseLivenessProbe = probe'})

discourseReadinessProbe :: Lens' DiscourseParameters ProbeParameters
discourseReadinessProbe =
  lens _discourseReadinessProbe (\discourseParameters' probe' -> discourseParameters' {_discourseReadinessProbe = probe'})

discourseCustomLivenessProbe :: Lens' DiscourseParameters ProbeParameters
discourseCustomLivenessProbe =
  lens _discourseCustomReadinessProbe (\discourseParameters' probe' -> discourseParameters' {_discourseCustomReadinessProbe = probe'})

discourseExtraEnvironmentVariables :: Lens' DiscourseParameters (Set EnvironmentVariable)
discourseExtraEnvironmentVariables =
  lens _discourseExtraEnvironmentVariables (\discourseParameters' env' -> discourseParameters' {_discourseExtraEnvironmentVariables = env'})

discourseExtraEnvVarsConfigMap :: Lens' DiscourseParameters V1ConfigMap
discourseExtraEnvVarsConfigMap =
  lens _discourseExtraEnvVarsConfigMap
    (\discourseParameters' configMap' -> discourseParameters' {_discourseExtraEnvVarsConfigMap = configMap'})

discourseExtraVolumeMounts :: Lens' DiscourseParameters (Set V1PersistentVolume)
discourseExtraVolumeMounts =
  lens _discourseExtraVolumeMounts
    (\discourseParameters' mounts' -> discourseParameters' {_discourseExtraVolumeMounts = mounts'})

discourseSkipInstall :: Lens' DiscourseParameters Bool
discourseSkipInstall =
  lens _discourseSkipInstall
    (\discourseParameters' skip' -> discourseParameters' {_discourseSkipInstall = skip'})






