module RedisParameters where

import Kubernetes.OpenAPI
import CommonTypes
import Lens.Micro
data RedisParameters = RedisParameters {
  _redisEnabled :: Bool
  , _redisUsePassword :: Bool
  , _redisPassword :: Password
  , _redisExistingSecret :: V1Secret
  , _redisExistingSecretPasswordKey :: K8sKey
  , _redisClusterEnabled :: Bool
  , _redisClusterMasterPersistenceEnabled :: Bool
  , _externalRedisHost :: HostName
  , _externalRedisPortNumber :: Port
  , _externalRedisPassword :: Password
  , _externalRedisExistingSecret :: V1Secret
  , _externalRedisExistingSecretPasswordKey :: K8sKey
}

{- |
  * Deploy redis containers - default : True
-}

redisEnabled :: Lens' RedisParameters Bool
redisEnabled =
  lens _redisEnabled (\redisParameters' enabled' -> redisParameters' {_redisEnabled = enabled'})

{- |
  * redis.usePassword Use password authentication false
-}
redisUsePassword :: Lens' RedisParameters Bool
redisUsePassword =
  lens _redisUsePassword (\redisParameters' usePassword' -> redisParameters' {_redisUsePassword = usePassword'})


redisPassword :: Lens' RedisParameters Password
redisPassword =
  lens _redisPassword (\redisParameters' password' -> redisParameters' {_redisPassword = password'})

redisExistingSecret :: Lens' RedisParameters V1Secret
redisExistingSecret =
  lens _redisExistingSecret (\redisParameters' secret' -> redisParameters' {_redisExistingSecret = secret'})

redisExistingSecretPasswordKey :: Lens' RedisParameters K8sKey
redisExistingSecretPasswordKey =
  lens _redisExistingSecretPasswordKey (\redisParameters' passwordKey' -> redisParameters' {_redisExistingSecretPasswordKey = passwordKey'})

redisClusterEnabled :: Lens' RedisParameters Bool
redisClusterEnabled =
  lens _redisClusterEnabled (\redisParameters' clusterEnabled' -> redisParameters' {_redisClusterEnabled = clusterEnabled'})

redisClusterMasterPersistenceEnabled :: Lens' RedisParameters Bool
redisClusterMasterPersistenceEnabled =
  lens _redisClusterMasterPersistenceEnabled (\redisParameters' masterPersistenceEnabled' -> redisParameters' {_redisClusterMasterPersistenceEnabled = masterPersistenceEnabled'})

externalRedisHost :: Lens' RedisParameters HostName
externalRedisHost =
  lens _externalRedisHost (\redisParameters' host' -> redisParameters' {_externalRedisHost = host'})

externalRedisPortNumber :: Lens' RedisParameters Port
externalRedisPortNumber =
  lens _externalRedisPortNumber (\redisParameters' port' -> redisParameters' {_externalRedisPortNumber = port'})

