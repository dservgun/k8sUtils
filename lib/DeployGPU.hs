{-# language OverloadedStrings #-}
{-# language InstanceSigs      #-}

module DeployGPU where

import CommonTypes
import PodParameters
import Data.Coerce
import Data.Default
import Data.Map
import Data.Text
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
  , _vendorName :: GPUVendor
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

