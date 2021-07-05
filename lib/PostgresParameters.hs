module PostgresParameters where

import           CommonTypes
import           ExternalDBParameters
import           Kubernetes.OpenAPI
import           Lens.Micro

data PostgresParameters = PostgresParameters {
  _postgresEnabled               :: Bool
  , _postgresqlUserName          :: UserName
  , _postgresqlPassword         :: Password
  , _postgresqlPostgresPassword :: Password
  , _postgresqlDatabaseName      :: DatabaseName
  , _postgresqlPersistenceEnabled :: Bool
  , _externalDBParameters        :: ExternalDBParameters
}

postgresEnabled :: Lens' PostgresParameters Bool
postgresEnabled =
  lens _postgresEnabled
    (\ aPostgresParameters' postgresEnabled' ->
      aPostgresParameters' {_postgresEnabled = postgresEnabled'})


postgresqlUserName :: Lens' PostgresParameters UserName
postgresqlUserName =
  lens _postgresqlUserName
    (\ aPostgresParameters' postgresqlUserName' ->
      aPostgresParameters' {_postgresqlUserName = postgresqlUserName'})

postgresqPassword :: Lens' PostgresParameters Password
postgresqPassword =
  lens _postgresqlPassword
    (\ aPostgresParameters' postgresqPassword' ->
      aPostgresParameters' {_postgresqlPassword = postgresqPassword'})

postgresqlPostgresPassword :: Lens' PostgresParameters Password
postgresqlPostgresPassword =
  lens _postgresqlPostgresPassword
    (\ aPostgresParameters' postgresqlPostgresPassword' ->
      aPostgresParameters' {_postgresqlPostgresPassword = postgresqlPostgresPassword'})

postgresqlDatabaseName :: Lens' PostgresParameters DatabaseName
postgresqlDatabaseName =
  lens _postgresqlDatabaseName
    (\ aPostgresParameters' postgresqlDatabaseName' ->
      aPostgresParameters' {_postgresqlDatabaseName = postgresqlDatabaseName'})

postgresPersistenceEnabled :: Lens' PostgresParameters Bool
postgresPersistenceEnabled =
  lens _postgresqlPersistenceEnabled
    (\ aPostgresParameters' postgresPersistenceEnabled' ->
      aPostgresParameters' {_postgresqlPersistenceEnabled = postgresPersistenceEnabled'})

externalDBParameters :: Lens' PostgresParameters ExternalDBParameters
externalDBParameters =
  lens _externalDBParameters
    (\ aPostgresParameters' externalDBParameters' ->
      aPostgresParameters' {_externalDBParameters = externalDBParameters'})


