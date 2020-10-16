{-# language OverloadedStrings #-}
{-# language InstanceSigs      #-}

module GPU.DeployGPU where

import CommonTypes
import PodParameters
import Data.Coerce
import Data.Default
import Data.Map
import Data.Text as Text
import Kubernetes.Client
import Kubernetes.OpenAPI
import Kubernetes.OpenAPI.API.AppsV1
import Kubernetes.OpenAPI.API.CoreV1
import Lens.Micro
import GHC.Natural


{-
apiVersion: v1
kind: Pod
metadata:
  name: cuda-vector-add
spec:
  restartPolicy: OnFailure
  containers:
    - name: cuda-vector-add
      # https://github.com/kubernetes/kubernetes/blob/v1.7.11/test/images/nvidia-cuda/Dockerfile
      image: "k8s.gcr.io/cuda-vector-add:v0.1"
      resources:
        limits:
          nvidia.com/gpu: 1 # requesting 1 GPU

-}

data GPUParameters = GPUParameters {
  _apiVersion :: APIVersion
  , _namespace :: Namespace
  , _metadataName :: Name
  , _podName :: PodName
  , _restartPolicy :: RestartPolicy
  , _cudaImage :: DockerImage
  , _vendor :: GPUVendor
  , _quantity :: Int
  }

instance Default GPUParameters where
  def =
    GPUParameters
      (def :: APIVersion)
      (def :: Namespace)
      (Name "cuda-vector-add")
      (PodName "cuda-vector-default")
      def
      (DockerImage "k8s.gcr.io/cuda-vector-add:v0.1")
      def
      1

instance PodParameters GPUParameters where
  apiVersion :: Lens' GPUParameters APIVersion
  apiVersion =
    lens _apiVersion (\gpuParameters' apiVersion' -> gpuParameters' {_apiVersion = apiVersion'})

  metadataName :: Lens' GPUParameters Name
  metadataName =
    lens _metadataName (\gpuParameters' metadataName' -> gpuParameters' {_metadataName = metadataName'})

  podName :: Lens' GPUParameters PodName
  podName =
    lens _podName (\gpuParameters' podName' -> gpuParameters' {_podName = podName'})

  podImage :: Lens' GPUParameters DockerImage
  podImage =
    lens _cudaImage (\gpuParameters' image' -> gpuParameters' {_cudaImage = image'})

  restartPolicy :: Lens' GPUParameters RestartPolicy
  restartPolicy =
    lens _restartPolicy (\gpuParameters' restartPolicy' -> gpuParameters' {_restartPolicy = restartPolicy'})


gpuVendor :: Lens' GPUParameters GPUVendor
gpuVendor =
  lens _vendor (\gpuParameters' vendor' -> gpuParameters' {_vendor = vendor'})

gpuQuantity :: Lens' GPUParameters Int
gpuQuantity =
  lens _quantity (\gpuParameters' quantity' -> gpuParameters' {_quantity = quantity'})

makePod :: GPUParameters -> V1Pod
makePod gpuParameters' =
  mkV1Pod &
    v1PodApiVersionL .~ (Just . coerce $ gpuParameters' ^. PodParameters.apiVersion) &
    v1PodKindL .~ (Just "Pod") &
    v1PodMetadataL .~ (Just mkV1ObjectMeta) &
    v1PodMetadataL . _Just . v1ObjectMetaNameL .~ (Just . coerce $ gpuParameters' ^. podName) &
    v1PodSpecL .~ (Just $ mkV1PodSpec []) &
    v1PodSpecL . _Just . v1PodSpecRestartPolicyL .~ (Just $ Text.pack . show $ gpuParameters' ^. restartPolicy) &
    v1PodSpecL . _Just . v1PodSpecContainersL .~ 
        [
          mkV1Container (coerce $ gpuParameters' ^. podName) &
            v1ContainerImageL .~ (Just . coerce $ gpuParameters' ^. podImage) &
            v1ContainerResourcesL .~ (Just mkV1ResourceRequirements) &
            v1ContainerResourcesL . _Just . v1ResourceRequirementsLimitsL .~ 
              ( -- TODO: this can get a bit hairy here.
                Just $ fromList 
                  [
                    ( ((show $ gpuParameters' ^. gpuVendor) <> ".com/gpu"), Quantity . Text.pack . show $ gpuParameters' ^. gpuQuantity)
                  ]
              )
        ]