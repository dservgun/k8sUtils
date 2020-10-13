# k8sUtils

## Introduction
A set of pods, deployments interacting with a kubernetes installation.

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

The pods heavily rely on [microlens](https://hackage.haskell.org/package/microlens) as the client libraries provide code for these lenses. Yes we all feel the churn every time the installation compiles for the first time.

### A brief introduction to microlens for k8s



