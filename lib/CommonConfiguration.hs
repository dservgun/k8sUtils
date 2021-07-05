module CommonConfiguration where

import           CommonTypes
import           Data.Set
import           ImageTypes
import           Kubernetes.OpenAPI
import           Lens.Micro
import           PersistenceParameters

data CommonConfiguration =
  CommonConfiguration {
    _replicaCount            :: ReplicaCount
    , _imageRegistry         :: DockerImageRegistry
    , _imageRepository       :: DockerImageRepository
    , _imageTag              :: ImageTag
    , _imagePullPolicy       :: ImagePullPolicy
    , _imageDebug            :: Bool
    , _imagePullSecrets      :: DockerImagePullSecrets
    , _nameOverride          :: NameOverride
    , _fullNameOverride      :: NameOverride
    , _extraVolumes          :: [V1VolumeMount]
    , _sidecarContainers     :: [SidecarContainer]
    , _initContainers        :: [V1Container]
    , _serviceAccount        :: ServiceAccountName
    , _createServiceAccount  :: Bool
    , _podSecurityContext    :: V1PodSecurityContext -- Use some sensible defaults.
    , _updateStrategy        :: UpdateStrategy
    , _podAnnotations        :: Set Annotation
    , _podLabels             :: Set Label
    , _commonAnnotations     :: Set Annotation
    , _commonLabels          :: Set Label
    , _persistenceParameters :: PersistenceParameters
    , _podAffinity           :: V1Affinity
    , _nodeSelector          :: Set Label -- Node labels for pod assignment.
    , _tolerations           :: Set V1Toleration
  }

replicaCount :: Lens' CommonConfiguration ReplicaCount
replicaCount =
  lens _replicaCount (\commonConfiguration' count' -> commonConfiguration' {_replicaCount = count'})

imageRegistry :: Lens' CommonConfiguration DockerImageRegistry
imageRegistry =
  lens _imageRegistry (\commonConfiguration' registry' -> commonConfiguration' {_imageRegistry = registry'})

imageRepository :: Lens' CommonConfiguration DockerImageRepository
imageRepository =
  lens _imageRepository (\commonConfiguration' repository' -> commonConfiguration' {_imageRepository = repository'})

imageTag :: Lens' CommonConfiguration ImageTag
imageTag =
  lens _imageTag (\commonConfiguration' imageTag' -> commonConfiguration' {_imageTag = imageTag'})

imagePullPolicy :: Lens' CommonConfiguration ImagePullPolicy
imagePullPolicy =
  lens _imagePullPolicy (\commonConfiguration' imagePullPolicy' -> commonConfiguration' {_imagePullPolicy = imagePullPolicy'})

imageDebug :: Lens' CommonConfiguration Bool
imageDebug =
  lens _imageDebug (\commonConfiguration' imageDebug' -> commonConfiguration' {_imageDebug = imageDebug'})

imagePullSecrets :: Lens' CommonConfiguration DockerImagePullSecrets
imagePullSecrets =
  lens _imagePullSecrets (\commonConfiguration' secrets' -> commonConfiguration' {_imagePullSecrets = secrets'})

nameOverride :: Lens' CommonConfiguration NameOverride
nameOverride =
  lens _nameOverride (\commonConfiguration' nameOverride' -> commonConfiguration' {_nameOverride = nameOverride'})

fullNameOverride :: Lens' CommonConfiguration NameOverride
fullNameOverride =
  lens _fullNameOverride (\commonConfiguration' fNameOverride' -> commonConfiguration' {_fullNameOverride = fNameOverride'})

extraVolumes :: Lens' CommonConfiguration [V1VolumeMount]
extraVolumes =
  lens _extraVolumes (\commonConfiguration' extraVolumes' -> commonConfiguration' {_extraVolumes = extraVolumes'})

sidecarContainers :: Lens' CommonConfiguration [SidecarContainer]
sidecarContainers =
  lens _sidecarContainers (\commonConfiguration' sidecarContainers' ->
    commonConfiguration' {_sidecarContainers = sidecarContainers'})

initContainers :: Lens' CommonConfiguration [V1Container]
initContainers =
  lens _initContainers (\commonConfiguration' initContainers' ->
    commonConfiguration' {_initContainers = initContainers'})

serviceAccount :: Lens' CommonConfiguration ServiceAccountName
serviceAccount =
  lens _serviceAccount (\commonConfiguration' serviceAccount' -> commonConfiguration' {_serviceAccount = serviceAccount'})

createServiceAccount :: Lens' CommonConfiguration Bool
createServiceAccount =
  lens _createServiceAccount (\commonConfiguration' createServiceAccount' ->
    commonConfiguration' {_createServiceAccount = createServiceAccount'})

podSecurityContext :: Lens' CommonConfiguration V1PodSecurityContext
podSecurityContext =
  lens _podSecurityContext (\commonConfiguration' securityContext' ->
    commonConfiguration' {_podSecurityContext = securityContext'})

updateStrategy :: Lens' CommonConfiguration UpdateStrategy
updateStrategy =
  lens _updateStrategy (\commonConfiguration' updateStrategy' ->
    commonConfiguration' {_updateStrategy = updateStrategy'})

podAnnotations :: Lens' CommonConfiguration (Set Annotation)
podAnnotations =
  lens _podAnnotations (\commonConfiguration' podAnnotations' ->
    commonConfiguration' {_podAnnotations = podAnnotations'})

podLabels :: Lens' CommonConfiguration (Set Label)
podLabels =
  lens _podLabels (\commonConfiguration' podLabels' ->
    commonConfiguration' {_podLabels = podLabels'})

commonAnnotations :: Lens' CommonConfiguration (Set Annotation)
commonAnnotations =
  lens _commonAnnotations (\commonConfiguration' annotations' ->
    commonConfiguration' {_commonAnnotations = annotations'})

commonLabels :: Lens' CommonConfiguration (Set Label)
commonLabels =
  lens _commonLabels (\commonConfiguration' commonLabels' ->
    commonConfiguration' {_commonLabels = commonLabels'})

persistenceParameters :: Lens' CommonConfiguration PersistenceParameters
persistenceParameters =
  lens _persistenceParameters (\commonConfiguration' persistenceParameters' ->
    commonConfiguration' {_persistenceParameters = persistenceParameters'})

podAffinity :: Lens' CommonConfiguration V1Affinity
podAffinity =
  lens _podAffinity (\commonConfiguration' affinity' ->
    commonConfiguration' {_podAffinity = affinity'})

nodeSelector :: Lens' CommonConfiguration (Set Label)
nodeSelector =
  lens _nodeSelector (\commonConfiguration' nodeSelector' ->
    commonConfiguration' {_nodeSelector = nodeSelector'})

tolerations :: Lens' CommonConfiguration (Set V1Toleration)
tolerations =
  lens _tolerations (\commonConfiguration' tolerations' ->
    commonConfiguration' {_tolerations = tolerations'})

