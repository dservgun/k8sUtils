module PersistenceParameters where

import Kubernetes.OpenAPI
import CommonTypes

data PersistenceParameters = PersistenceParameters {
  _persistenceEnabled :: Bool
  , _persistenceStorageClass :: V1StorageClass
  , _persistenceExistingClaim :: V1PersistentVolumeClaim
  , _persistenceAccessMode :: StorageAccessMode
  , _persistenceSize :: Quantity
}
