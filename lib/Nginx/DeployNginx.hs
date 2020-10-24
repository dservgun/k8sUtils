{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

-- @Author: dinkar
-- @Date:   2020-10-11 21:34:32
-- @Last Modified by:   dinkar
-- @Last Modified time: 2020-10-16 15:13:17

module Nginx.DeployNginx
  (
    makeDeployment
    , NginxParameters
  )
where

import           CommonTypes
import           Data.Coerce
import           Data.Default
import           Data.Map
import           Data.Text
import           DeploymentParameters
import           GHC.Natural
import           Kubernetes.Client
import           Kubernetes.OpenAPI
import           Kubernetes.OpenAPI.API.AppsV1
import           Kubernetes.OpenAPI.API.CoreV1
import           Lens.Micro

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
  _apiVersion      :: APIVersion
  , _namespace     :: Namespace
  , _metadataName  :: Name
  , _appName       :: AppName
  , _replicas      :: ReplicaCount
  , _nginxImage    :: DockerImage
  , _containerPort :: ContainerPort
} deriving (Show)

instance Default NginxParameters where
  def =
    NginxParameters
      (APIVersion "apps/v1")
      (Namespace "default")
      (Name "nginx-deployment")
      (AppName "nginx")
      (ReplicaCount 2)
      (DockerImage "nginx:1.7.9")
      (ContainerPort 80)

instance DeploymentParameters NginxParameters where
  apiVersion :: Lens' NginxParameters APIVersion
  apiVersion =
    lens _apiVersion (\nginxParameters' apiVersion' -> nginxParameters' {_apiVersion = apiVersion'})

  namespace :: Lens' NginxParameters Namespace
  namespace =
    lens _namespace (\nginxParameters' namespace' -> nginxParameters' {_namespace = namespace'})

  metadataName :: Lens' NginxParameters Name
  metadataName =
    lens _metadataName (\nginxParameters' metadataName' -> nginxParameters' {_metadataName = metadataName'})

  appName :: Lens' NginxParameters AppName
  appName =
    lens _appName (\nginxParameters' appName' -> nginxParameters' {_appName = appName'})

  replicas :: Lens' NginxParameters ReplicaCount
  replicas =
    lens _replicas (\nginxParameters' replicaCount' -> nginxParameters' {_replicas = replicaCount'})

  deploymentImage :: Lens' NginxParameters DockerImage
  deploymentImage =
    lens _nginxImage (\nginxParameters' nginxImage' -> nginxParameters' {_nginxImage = nginxImage'})

  containerPort :: Lens' NginxParameters ContainerPort
  containerPort =
    lens _containerPort (\nginxParameters' containerPort' -> nginxParameters' {_containerPort = containerPort'})


makeDeployment :: NginxParameters -> V1Deployment
makeDeployment nginxParams =
  mkV1Deployment &
    v1DeploymentMetadataL .~ (Just mkV1ObjectMeta) &
    v1DeploymentMetadataL . _Just . v1ObjectMetaNameL .~ (Just . coerce $ nginxParams ^. metadataName) &
    v1DeploymentApiVersionL .~ (Just . coerce $ nginxParams ^. DeploymentParameters.apiVersion) &
    v1DeploymentKindL .~ (Just "deployment") &
    v1DeploymentSpecL .~ (Just $ mkV1DeploymentSpec mkV1LabelSelector mkV1PodTemplateSpec) &
    v1DeploymentSpecL . _Just . v1DeploymentSpecSelectorL . v1LabelSelectorMatchLabelsL .~ (Just $ fromList [("app", coerce $ nginxParams ^. appName)]) &
    v1DeploymentSpecL . _Just . v1DeploymentSpecReplicasL .~ (Just . naturalToInt . coerce $ nginxParams ^. replicas) &
    v1DeploymentSpecL . _Just . v1DeploymentSpecTemplateL . v1PodTemplateSpecSpecL .~ (Just $ mkV1PodSpec [mkV1Container "nginx"]) &
    v1DeploymentSpecL . _Just . v1DeploymentSpecTemplateL . v1PodTemplateSpecSpecL . _Just . v1PodSpecContainersL .~
      [
        mkV1Container "nginx" &
          v1ContainerNameL .~ (coerce $ nginxParams ^. appName) &
          v1ContainerPortsL .~ (Just [mkV1ContainerPort $ naturalToInt . coerce $ nginxParams ^. containerPort]) &
          v1ContainerImageL .~ (Just $ coerce $ nginxParams ^. deploymentImage)
      ]
