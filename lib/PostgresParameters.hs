module PostgresParameters where

import           CommonTypes
import           ExternalDBParameters
import           Kubernetes.OpenAPI
import           Lens.Micro

data PostgresParameters = PostgresParameters {
  _postgresEnabled               :: Bool
  , _postgresqlUserName          :: UserName
  , _postgressqlPassword         :: Password
  , _postgressqlPostgresPassword :: Password
  , _postgresqlExistingSecret    :: V1Secret
  , _postgresqlDatabaseName      :: DatabaseName
  , _postgresqlPersistencEnabled :: Bool
  , _externalDBParameters        :: ExternalDBParameters
}
