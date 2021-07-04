# k8sUtils

## Introduction
A set of pods, deployments interacting with a kubernetes installation.

## Inheritance
Configurations need to support a form of inheritance so that each configuration can be modified independently. Within [k8sUtils](https://github.com/dservgun/k8sUtils) we borrow the concepts of inclusion and inheritance through composition. For example
`haskell
  For example, DiscourseConfiguration includes CommonConfiguration to use commonly defined labels, registry.

`
This approach does have its disadvantages and something we will revisit.
## Some conventions
We like to flatten hierarchies unless needed. For example,

`haskell
  module CommonTypes where
  ...
`

is preferable to

`haskell
  module Data.CommonTypes where ...
`

We may still need to maintain Internal modules to layer the interface; so much for convention.

## Design approach

The pods rely on [microlens](https://hackage.haskell.org/package/microlens) as the client libraries provide code for these lenses. Yes we all feel the churn every time the installation compiles for the first time.

### A brief introduction to microlens for k8s
Let us take the following example,

```
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
```

The corresponding version in haskell.The size of the function is almost the same size as the configuration file, with added functions for lenses, that we are not showing below.
```
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
```

### Notes
The code could be much simpler if we allowed for the entire lens machinery in this library. A lot of microlens boilerplate can be reduced. We are going to decide to using microlens till we really get tired of writing these boilerplate lens functions or perhaps write a generator that we can use.

### References
[How to add tracers](https://vadosware.io/post/better-k8s-monitoring-part-3-adding-tracing-with-opentracing-and-jaeger/)