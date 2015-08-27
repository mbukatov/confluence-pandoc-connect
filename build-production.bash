#!/bin/bash -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
echo "DIR: ${DIR}"
set -e

# Usage: build-production
IMAGE_TAG=`git describe --always`
IMAGE_BUILD_TAG="${IMAGE_TAG}-build"
DOCKER_CMD=${DOCKER_CMD:-docker}

echo "## Building code in image: $IMAGE_BUILD_TAG"
${DOCKER_CMD} build --rm=true --tag="$IMAGE_BUILD_TAG" "${DIR}"
echo "## Built code in image: $IMAGE_BUILD_TAG"
echo "## Generating service descriptor for version $IMAGE_TAG"
RELEASE_VERSION=${IMAGE_TAG} bash generate-service-descriptor.bash
echo "## Building production image: $IMAGE_TAG"
bash to-production.bash "$IMAGE_BUILD_TAG" "$IMAGE_TAG"
echo "## Built production image: $IMAGE_TAG"

exit 0
