-- @Author: dinkar
-- @Date:   2020-10-17 18:46:10
-- @Last Modified by:   dinkar
-- @Last Modified time: 2020-10-22 23:34:11
{-# language OverloadedStrings #-}
{-# language InstanceSigs      #-}

module Discourse.ConfigMap where

import CommonTypes
import Data.Coerce
import Data.Map as Map
import Kubernetes.OpenAPI
import Lens.Micro
import Discourse.DiscourseConfiguration

  -- POSTGRESQL_HOST: postgresql
  -- POSTGRESQL_ROOT_USER: postgres
  -- POSTGRESQL_ROOT_PASSWORD: "redacted"
  -- POSTGRESQL_CLIENT_CREATE_DATABASE_NAME: bitnami_discourse
  -- POSTGRESQL_CLIENT_CREATE_DATABASE_USERNAME: bn_discourse
  -- POSTGRESQL_CLIENT_CREATE_DATABASE_PASSWORD: "redacted"

  -- DISCOURSE_POSTGRESQL_NAME: bitnami_discourse
  -- DISCOURSE_POSTGRESQL_USERNAME: bn_discourse
  -- DISCOURSE_POSTGRESQL_PASSWORD: "bn_discourse"
  -- DISCOURSE_SITENAME: "My Forum"
  -- DISCOURSE_HOSTNAME: myhost
  -- DISCOURSE_USERNAME: admin
  -- DISCOURSE_EMAIL: email@example.com
  -- DISCOURSE_PASSWORD: "redacted"
  -- DISCOURSE_HOST: discourse
  -- DISCOURSE_PORT: "3000"

  -- REDIS_HOST: redis
  -- REDIS_PORT_NUMBER: "6379"

  -- SMTP_HOST: email-smtp.us-west-2.amazonaws.com
  -- SMTP_PORT: "587"
  -- SMTP_USER: "redacted"
  -- SMTP_PASSWORD: "redacted"
makeConfig :: DiscourseParameters -> V1ConfigMap
makeConfig discourseParameters =
  mkV1ConfigMap &
    v1ConfigMapApiVersionL .~ (Just "v1") &
    v1ConfigMapKindL .~ (Just "ConfigMap") &
    v1ConfigMapMetadataL .~ (Just mkV1ObjectMeta) &
    v1ConfigMapMetadataL . _Just . v1ObjectMetaNameL .~ (Just . coerce $ discourseParameters ^. discourseFullName) &
    v1ConfigMapDataL .~ (Just $ Map.fromList [])
    