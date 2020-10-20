-- @Author: dinkar
-- @Date:   2020-10-19 20:28:14
-- @Last Modified by:   dinkar
-- @Last Modified time: 2020-10-19 21:19:06

module IngressParameters where

import Lens.Micro
import Data.Set
import CommonTypes
import Kubernetes.OpenAPI
import Data.Map

data IngressParameters = IngressParameters {
  _ingressEnabled :: Bool
  , _ingressCertificateManager :: Set Annotation -- TODO: What is the annotation here?
  , _ingressHostName :: HostName
  , _ingressTLS :: Bool
  , _ingressAnnotation :: Set Annotation
  , _ingressExtraHosts :: Map Host (Maybe V1Secret)
}

ingressEnabled :: Lens' IngressParameters Bool
ingressEnabled =
  lens _ingressEnabled 
    (\ingressParameters' enabled' -> ingressParameters' {_ingressEnabled = enabled'})

ingressCertificateManager :: Lens' IngressParameters (Set Annotation)
ingressCertificateManager =
  lens _ingressCertificateManager 
    (\ingressParameters' cert' -> ingressParameters' {_ingressCertificateManager = cert'})

ingressHostName :: Lens' IngressParameters HostName
ingressHostName =
  lens _ingressHostName
    (\ingressParameters' hostName' -> ingressParameters' {_ingressHostName = hostName'})

ingressTLS :: Lens' IngressParameters Bool
ingressTLS =
  lens _ingressTLS
    (\ingressParameters' tls' -> ingressParameters' {_ingressTLS = tls'})

ingressAnnotation :: Lens' IngressParameters (Set Annotation)
ingressAnnotation =
  lens _ingressAnnotation
    (\ingressParameters' annotations' -> ingressParameters' {_ingressAnnotation = annotations'})

ingressExtraHosts :: Lens' IngressParameters (Map Host (Maybe V1Secret))
ingressExtraHosts =
  lens _ingressExtraHosts
    (\ingressParameters' hosts' ->
      ingressParameters' {_ingressExtraHosts = hosts'})
