-- @Author: dinkar
-- @Date:   2020-10-19 20:28:14
-- @Last Modified by:   dinkar
-- @Last Modified time: 2021-07-04 22:58:43

module IngressParameters where

import           CommonTypes
import           Data.Map
import           Data.Set
import           Kubernetes.OpenAPI
import           Lens.Micro

data IngressParameters = IngressParameters {
  _ingressEnabled              :: Bool
  , _ingressCertificateManager :: Set Annotation -- TODO: What is the annotation here?
  , _ingressHostName           :: HostName
  , _ingressTLS                :: Bool
  , _ingressAnnotation         :: Set Annotation

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
