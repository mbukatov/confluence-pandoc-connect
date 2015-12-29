#! /bin/bash -e

REPOSITORY='docker.atlassian.io'
IMAGE_NAME='atlassian/confluence-pandoc-connect'
IMAGE_TAG=`git describe --always`
DOCKER_TAG="${REPOSITORY}/${IMAGE_NAME}:${IMAGE_TAG}"
DOCKER_BUILD_TAG="${DOCKER_TAG}-build"
STACK_RESOLVER="lts-3.19/7.10.2"
STACK_BINARY_PATH=".stack-work/install/x86_64-linux/${STACK_RESOLVER}/bin"

echo "# Building the binary with tag ${DOCKER_BUILD_TAG}"
docker build --rm=true --tag=${DOCKER_BUILD_TAG} -f Dockerfile ${DOCKER_BUILD_EXTRA_ARGS} .

echo "# Extracting the built binary"
container_id=$(docker create ${DOCKER_BUILD_TAG})
docker cp ${container_id}:/build/${STACK_BINARY_PATH}/hidden-charlie - > build-bin.tar
docker rm -v ${container_id}
mkdir build-bin
cd build-bin
tar -xf ../build-bin.tar
cd ..

echo "# Creating a runtime image with the built binary with tag ${DOCKER_TAG}"
docker build --rm=true --tag=${DOCKER_TAG} -f Dockerfile-run .

echo "# Pushing the image upstream"
docker push ${DOCKER_TAG}
echo "# Pushing the build image upstream for reuse"
docker push ${DOCKER_BUILD_TAG}
