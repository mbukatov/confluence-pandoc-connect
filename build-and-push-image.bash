#! /bin/bash -e

REPOSITORY='docker.atlassian.io'
IMAGE_NAME='atlassian/confluence-pandoc-connect'
IMAGE_TAG=`git describe --always`
DOCKER_TAG="${REPOSITORY}/${IMAGE_NAME}:${IMAGE_TAG}"

echo "# Building the binary with tag ${DOCKER_TAG}"
docker build --rm=true --tag=${DOCKER_TAG} -f Dockerfile .

echo "# Extracting the built binary"
container_id=$(docker create ${DOCKER_TAG})
docker cp ${container_id}:/build/.stack-work/install/x86_64-linux/lts-3.17/7.10.2/bin/hidden-charlie - > build-bin.tar
docker rm -v ${container_id}
mkdir build-bin
cd build-bin
tar -xf ../build-bin.tar
cd ..

echo "# Creating a runtime image with the built binary"
docker build --rm=true --tag=${DOCKER_TAG} -f Dockerfile-run .

echo "# Pushing the image upstream"
docker push ${DOCKER_TAG}
