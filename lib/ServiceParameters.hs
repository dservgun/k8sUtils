module ServiceParameters where

import           CommonTypes
import           Data.IP
import           Data.Set
import           Kubernetes.OpenAPI
import           Lens.Micro

data ServiceParameters = ServiceParameters {
  _serviceType                       :: CustomServiceType
  , _servicePort                     :: Port
  , _serviceNodePort                 :: Port
  , _serviceLoadBalancerIP           :: IP
  , _serviceExternalTrafficPolicy    :: ExternalTrafficPolicy
  , _serviceAnnotations              :: Set Annotation
  , _serviceLoadBalancerSourceRanges :: Set LoadBalancerSourceRange
  , _serviceExtraPorts               :: [Port]
  , _serviceHttpPort                 :: Port
}

serviceType :: Lens' ServiceParameters CustomServiceType
serviceType =
  lens _serviceType (\serviceParameters' serviceType' -> serviceParameters' {_serviceType = serviceType'})

servicePort :: Lens' ServiceParameters Port
servicePort =
  lens _servicePort (\serviceParameters' port' -> serviceParameters' {_servicePort = port'})

serviceNodePort :: Lens' ServiceParameters Port
serviceNodePort =
  lens _serviceNodePort (\serviceParameters' port' -> serviceParameters' {_serviceNodePort = port'})

serviceLoadBalancerIP :: Lens' ServiceParameters IP
serviceLoadBalancerIP =
  lens _serviceLoadBalancerIP (\serviceParameters' ip' -> serviceParameters' {_serviceLoadBalancerIP = ip'})

serviceExternalTrafficPolicy :: Lens' ServiceParameters ExternalTrafficPolicy
serviceExternalTrafficPolicy =
  lens _serviceExternalTrafficPolicy (\serviceParameters' policy' -> serviceParameters' {_serviceExternalTrafficPolicy = policy'})

serviceAnnotations :: Lens' ServiceParameters (Set Annotation)
serviceAnnotations =
  lens _serviceAnnotations (\serviceParameters' annotations' -> serviceParameters' {_serviceAnnotations = annotations'})

serviceLoadBalancerSourceRanges :: Lens' ServiceParameters (Set LoadBalancerSourceRange)
serviceLoadBalancerSourceRanges =
  lens _serviceLoadBalancerSourceRanges (\serviceParameters' ranges' -> serviceParameters' {_serviceLoadBalancerSourceRanges = ranges'})

serviceExtraPorts :: Lens' ServiceParameters [Port]
serviceExtraPorts =
  lens _serviceExtraPorts (\serviceParameters' extraPorts' -> serviceParameters' {_serviceExtraPorts = extraPorts'})

serviceHttpPort :: Lens' ServiceParameters Port
serviceHttpPort =
  lens _serviceHttpPort (\serviceParameters' port' -> serviceParameters' {_serviceHttpPort = port'})

