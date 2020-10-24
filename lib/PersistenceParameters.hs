module PersistenceParameters where

import           CommonTypes
import           Kubernetes.OpenAPI
import           Lens.Micro

data PersistenceParameters = PersistenceParameters {
  _persistenceEnabled         :: Bool
  , _persistenceStorageClass  :: V1StorageClass
  , _persistenceExistingClaim :: V1PersistentVolumeClaim
  , _persistenceAccessMode    :: StorageAccessMode
  , _persistenceSize          :: Quantity
}

persistenceEnabled :: Lens' PersistenceParameters Bool
persistenceEnabled =
  lens _persistenceEnabled (\persistenceParameters' enabled' -> persistenceParameters' {_persistenceEnabled = enabled'})

persistenceStorageClass :: Lens' PersistenceParameters V1StorageClass
persistenceStorageClass =
  lens _persistenceStorageClass
    (\persistenceParameters' storageClass' -> persistenceParameters' {_persistenceStorageClass = storageClass'})

persistenceExistingClaim :: Lens' PersistenceParameters V1PersistentVolumeClaim
persistenceExistingClaim =
  lens _persistenceExistingClaim
    (\persistenceParameters' existingClaim' -> persistenceParameters' {_persistenceExistingClaim = existingClaim'})

persistenceAccessMode :: Lens' PersistenceParameters StorageAccessMode
persistenceAccessMode =
  lens _persistenceAccessMode
    (\persistenceParameters' accessMode' -> persistenceParameters' {_persistenceAccessMode = accessMode'})

persistenceSize :: Lens' PersistenceParameters Quantity
persistenceSize =
  lens _persistenceSize (\persistenceParameters' size' -> persistenceParameters' {_persistenceSize = size'})

