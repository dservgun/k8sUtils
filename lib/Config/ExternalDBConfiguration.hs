-- @Author: dinkar
-- @Date:   2021-07-04 09:56:31
-- @Last Modified by:   dinkar
-- @Last Modified time: 2021-07-04 11:46:06
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module Config.ExternalDBConfiguration where

import Data.Text as Text
import Dhall
import ExternalDBParameters
import CommonTypes
import Kubernetes.OpenAPI

instance FromDhall ExternalDBParameters
instance FromDhall HostName
instance FromDhall Port
instance FromDhall UserName
instance FromDhall Password
instance FromDhall DatabaseName


loadExternalDBParameters :: FilePath -> IO ExternalDBParameters
loadExternalDBParameters aFilePath = do
  db <- input auto (Text.pack aFilePath)
  return (db :: ExternalDBParameters)
