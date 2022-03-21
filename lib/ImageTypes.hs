{-# LANGUAGE DerivingVia #-}

module ImageTypes where

import           Data.Text

newtype DockerImageRegistry = DockerImageRegistry Text
  deriving Show via Text
  deriving Read via Text

newtype DockerImagePullSecrets = DockerImagePullSecrets Text
  deriving Show via Text
  deriving Read via Text

data ImagePullPolicy =
  Always | Never | IfNotPresent
  deriving (Show, Read, Enum, Bounded, Eq, Ord)

newtype DockerImageRepository = DockerImageRepository Text
  deriving Show via Text
  deriving Read via Text

newtype ImageTag = ImageTag Text
  deriving Show via Text
  deriving Read via Text
