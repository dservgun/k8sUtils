-- @Author: dinkar
-- @Date:   2020-10-18 20:53:29
-- @Last Modified by:   dinkar
-- @Last Modified time: 2020-10-18 20:54:40
module SidekiqParameters where

import Lens.Micro
import CommonTypes
import ProbeParameters
import Data.Set
import Kubernetes.OpenAPI

data SidekiqParameters = SidekiqParameters {
  _sidekiqSecurityContext :: V1SecurityContext
  , _sidekiqCommand :: CustomCommand
  , _sidekiqArgs :: [Argument]
  , _sidekiqResources :: [V1ResourceRequirements]
  , _sidekiqLivenessProbe :: ProbeParameters
  , _sidekiqReadinessProbe :: ProbeParameters
  , _sidekiqExtraEnvironmentVariables :: Set EnvironmentVariable
  , _sidekiqExtraEnvVarsConfigMaps :: V1ConfigMap
  , _sidekiqExtraEnvVarsSecret :: V1Secret
  , _sidekiqExtraVolumeMounts :: Set V1PersistentVolume
}
