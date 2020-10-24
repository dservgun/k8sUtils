module ExternalDBParameters where

import           CommonTypes
import           Kubernetes.OpenAPI
import           Lens.Micro

data ExternalDBParameters = ExternalDBParameters {
  _hostName                         :: HostName
  , _portNumber                     :: Port
  , _externalUserName               :: UserName
  , _externalPassword               :: Password
  , _externalPostgresUser           :: UserName
  , _externalPostgresPassword       :: Password
  , _externalPostgresExistingSecret :: V1Secret
  , _externalDatabase               :: DatabaseName
}

hostName :: Lens' ExternalDBParameters HostName
hostName =
  lens _hostName (\externalDBParameters' hostName' -> externalDBParameters' {_hostName = hostName'})

portNumber :: Lens' ExternalDBParameters Port
portNumber =
  lens _portNumber (\externalDBParameters' port' -> externalDBParameters' {_portNumber = port'})

externalUserName :: Lens' ExternalDBParameters UserName
externalUserName =
  lens _externalUserName (\externalDBParameters' userName' -> externalDBParameters' {_externalUserName = userName'})

externalPassword :: Lens' ExternalDBParameters Password
externalPassword =
  lens _externalPassword (\externalDBParameters' pwd' -> externalDBParameters' {_externalPassword = pwd'})

externalPostgresUser :: Lens' ExternalDBParameters UserName
externalPostgresUser =
  lens _externalPostgresUser
    (\externalDBParameters' pgUser' -> externalDBParameters' {_externalPostgresUser = pgUser'})

externalPostgresPassword :: Lens' ExternalDBParameters Password
externalPostgresPassword =
  lens _externalPostgresPassword
    (\externalDBParameters' pwd' -> externalDBParameters' {_externalPostgresPassword = pwd'})

externalPostgresExistingSecret :: Lens' ExternalDBParameters V1Secret
externalPostgresExistingSecret =
  lens _externalPostgresExistingSecret (\externalDBParameters' secret' -> externalDBParameters' {_externalPostgresExistingSecret = secret'})

externalDatabase :: Lens' ExternalDBParameters DatabaseName
externalDatabase =
  lens _externalDatabase (\externalDBParameters' dbName' -> externalDBParameters' {_externalDatabase = dbName'})
