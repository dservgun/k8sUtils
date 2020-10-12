{-# language OverloadedStrings #-}
{-# language DerivingVia       #-}

-- @Author: dinkar
-- @Date:   2020-10-11 23:23:38
-- @Last Modified by:   dinkar
-- @Last Modified time: 2020-10-11 23:52:19
module CommonTypes where

import Data.Text
import Numeric.Natural


newtype AppName = AppName Text
  deriving Show via Text
  deriving Read via Text
newtype APIVersion = APIVersion Text
  deriving Show via Text
  deriving Read via Text

newtype ReplicaCount = ReplicaCount Natural
  deriving Show via Natural
  deriving Read via Natural

newtype DockerImage = DockerImage Text
  deriving Show via Text
  deriving Read via Text

newtype ContainerPort = ContainerPort Natural
  deriving Show via Natural 
  deriving Read via Natural

