-- @Author: dinkar
-- @Date:   2020-10-18 20:38:01
-- @Last Modified by:   dinkar
-- @Last Modified time: 2020-10-18 20:50:23
module ProbeParameters where

import Numeric.Natural
import Lens.Micro

data ProbeParameters = ProbeParameters {
    _probeEnabled :: Bool
    , _probeInitialDelaySeconds :: Natural
    , _probePeriodSeconds :: Natural
    , _probeTimeoutSeconds :: Natural
    , _probeFailureThreshold :: Natural
    , _probeSuccessThreshold :: Natural
}

probeEnabled :: Lens' ProbeParameters Bool
probeEnabled =
  lens _probeEnabled (\probeParameters' probeEnabled' -> probeParameters' {_probeEnabled = probeEnabled'})

probeInitialDelaySeconds :: Lens' ProbeParameters Natural
probeInitialDelaySeconds =
  lens _probeInitialDelaySeconds (\probeParameters' seconds' -> probeParameters' {_probeInitialDelaySeconds = seconds'})

probePeriodSeconds :: Lens' ProbeParameters Natural
probePeriodSeconds =
  lens _probePeriodSeconds (\probeParameters' seconds' -> probeParameters' {_probePeriodSeconds = seconds'})

probeTimeoutSeconds :: Lens' ProbeParameters Natural
probeTimeoutSeconds =
  lens _probeTimeoutSeconds (\probeParameters' seconds' -> probeParameters' {_probeTimeoutSeconds = seconds'})

probeFailureThreshold :: Lens' ProbeParameters Natural
probeFailureThreshold =
  lens _probeFailureThreshold (\probeParameters' threshold' -> probeParameters' {_probeFailureThreshold = threshold'})

probeSuccessThreshold :: Lens' ProbeParameters Natural
probeSuccessThreshold =
  lens _probeSuccessThreshold (\probeParameters' threshold' -> probeParameters' {_probeSuccessThreshold = threshold'})

