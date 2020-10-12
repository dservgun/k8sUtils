{-# language OverloadedStrings #-}
{-# language DerivingVia       #-}

-- @Author: dinkar
-- @Date:   2020-10-11 21:34:32
-- @Last Modified by:   dinkar
-- @Last Modified time: 2020-10-11 23:54:26

module Nginx where

import CommonTypes
import Kubernetes.Client
import Kubernetes.OpenAPI
import Kubernetes.OpenAPI.API.AppsV1
import Kubernetes.OpenAPI.API.CoreV1
import Lens.Micro
import Data.Default
import Data.Map
import Data.Text
import Data.Coerce

{--

apiVersion: apps/v1 # for versions before 1.9.0 use apps/v1beta2
kind: Deployment
metadata:
  name: nginx-deployment
spec:
  selector:
    matchLabels:
      app: nginx
  replicas: 2 # tells deployment to run 2 pods matching the template
  template:
    metadata:
      labels:
        app: nginx
    spec:
      containers:
      - name: nginx
        image: nginx:1.7.9
        ports:
        - containerPort: 80
-}


data NginxParameters = NginxParameters {
  _apiVersion :: APIVersion
  , _namespace :: Namespace
  , _metadataName :: Name
  , _appName :: AppName
  , _replicas :: ReplicaCount
  , _image :: DockerImage
  , _containerPort :: ContainerPort
} deriving (Show)

-- Lenses
apiVersion :: Lens' NginxParameters APIVersion
apiVersion =
  lens _apiVersion (\nginxParameters' apiVersion' -> nginxParameters' {_apiVersion = apiVersion'})

appName :: Lens' NginxParameters AppName
appName =
  lens _appName (\nginxParameters' appName' -> nginxParameters' {_appName = appName'})


makeDeployment :: NginxParameters -> V1Deployment
makeDeployment nginxParams =
  mkV1Deployment &
    v1DeploymentMetadataL .~ (Just mkV1ObjectMeta) &
    v1DeploymentMetadataL . _Just . v1ObjectMetaNameL .~ (Just undefined) &
    v1DeploymentApiVersionL .~ (Just . coerce $ nginxParams ^. Nginx.apiVersion) &
    v1DeploymentKindL .~ (Just "deployment") &
    v1DeploymentSpecL .~ (Just $ mkV1DeploymentSpec mkV1LabelSelector mkV1PodTemplateSpec) &
    v1DeploymentSpecL . _Just . v1DeploymentSpecSelectorL . v1LabelSelectorMatchLabelsL .~ (Just $ fromList [("app", coerce $ nginxParams ^. appName)])