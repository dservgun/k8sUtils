-- @Author: dinkar
-- @Date:   2020-10-18 20:53:29
-- @Last Modified by:   dinkar
-- @Last Modified time: 2020-10-19 15:56:51
module SidekiqParameters where

import           CommonTypes
import           Data.Set
import           Kubernetes.OpenAPI
import           Lens.Micro
import           ProbeParameters


data SidekiqParameters = SidekiqParameters {
  _sidekiqSecurityContext             :: V1SecurityContext
  , _sidekiqCommand                   :: CustomCommand
  , _sidekiqArgs                      :: [Argument]
  , _sidekiqResources                 :: [V1ResourceRequirements]
  , _sidekiqLivenessProbe             :: LivenessProbe
  , _sidekiqReadinessProbe            :: ReadinessProbe
  , _sidekiqExtraEnvironmentVariables :: Set EnvironmentVariable
  , _sidekiqExtraEnvVarsConfigMaps    :: V1ConfigMap
  , _sidekiqExtraEnvVarsSecret        :: V1Secret
  , _sidekiqExtraVolumeMounts         :: Set V1PersistentVolume
}

sidekiqSecurityContext :: Lens' SidekiqParameters V1SecurityContext
sidekiqSecurityContext =
  lens _sidekiqSecurityContext
    (\sidekiqParameters' context' -> sidekiqParameters' {_sidekiqSecurityContext = context'})

sidekiqCommand :: Lens' SidekiqParameters CustomCommand
sidekiqCommand =
  lens _sidekiqCommand
    (\sidekiqParameters' command' -> sidekiqParameters' {_sidekiqCommand = command'})

sidekiqArgs :: Lens' SidekiqParameters [Argument]
sidekiqArgs =
  lens _sidekiqArgs
    (\sidekiqParameters' args' -> sidekiqParameters' {_sidekiqArgs = args'})

sidekiqResources :: Lens' SidekiqParameters [V1ResourceRequirements]
sidekiqResources =
  lens _sidekiqResources
    (\sidekiqParameters' resources' -> sidekiqParameters' {_sidekiqResources = resources'})

sidekiqLivenessProbe :: Lens' SidekiqParameters LivenessProbe
sidekiqLivenessProbe =
  lens _sidekiqLivenessProbe
    (\sidekiqParameters' probe' -> sidekiqParameters' {_sidekiqLivenessProbe = probe'})

sidekiqReadinessProbe :: Lens' SidekiqParameters ReadinessProbe
sidekiqReadinessProbe =
  lens _sidekiqReadinessProbe
    (\sidekiqParameters' probe' -> sidekiqParameters' {_sidekiqReadinessProbe = probe'})

sidekiqExtraEnvironmentVariables :: Lens' SidekiqParameters (Set EnvironmentVariable)
sidekiqExtraEnvironmentVariables =
  lens _sidekiqExtraEnvironmentVariables
    (\sidekiqParameters' vars' -> sidekiqParameters' {_sidekiqExtraEnvironmentVariables = vars'})

sidekiqExtraEnvVarsConfigMaps :: Lens' SidekiqParameters V1ConfigMap
sidekiqExtraEnvVarsConfigMaps =
  lens _sidekiqExtraEnvVarsConfigMaps
    (\sidekiqParameters' maps' -> sidekiqParameters' {_sidekiqExtraEnvVarsConfigMaps = maps'})

sidekiqExtraEnvVarsSecret :: Lens' SidekiqParameters V1Secret
sidekiqExtraEnvVarsSecret =
  lens _sidekiqExtraEnvVarsSecret
    (\sidekiqParameters' secrets' -> sidekiqParameters' {_sidekiqExtraEnvVarsSecret = secrets'})

sidekiqExtraVolumeMounts :: Lens' SidekiqParameters (Set V1PersistentVolume)
sidekiqExtraVolumeMounts =
  lens _sidekiqExtraVolumeMounts
    (\sidekiqParameters' mounts' -> sidekiqParameters' {_sidekiqExtraVolumeMounts = mounts'})
