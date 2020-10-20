module PostgresParameters where

import Lens.Micro
import CommonTypes
import Kubernetes.OpenAPI
import ExternalDBParameters

data PostgresParameters = PostgresParameters {
  _postgresEnabled :: Bool
  , _postgresqlUserName :: UserName
  , _postgressqlPassword :: Password
  , _postgressqlPostgresPassword :: Password
  , _postgresqlExistingSecret :: V1Secret
  , _postgresqlDatabaseName :: DatabaseName
  , _postgresqlPersistencEnabled :: Bool
  , _externalDBParameters :: ExternalDBParameters
}
