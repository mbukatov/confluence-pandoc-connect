# The development docker file for the Confluence Pandoc Connect Haskell project.
# This docker file is designed to help us build the production executables but
# should not be used to actually generate the production docker images. Instead
# we should make a new production image that takes the executables from this
# image and only includes them. That way we do not need to carry the entire
# Haskell platform with us into production. Just the small set of required
# dependencies.

FROM phadej/ghc:7.10.1
MAINTAINER Avi Knoll <aknoll@atlassian.com>

# Expose the default port, port 8000
EXPOSE 8000

# Install the missing packages
USER root
RUN apt-get update && apt-get install -y libpq-dev

# Copy our context into the build directory and start working from there
ADD .   /root/build

# Setup the Haskell Envoronment
WORKDIR /root/build
ENV LANG en_US.UTF-8 # See: https://github.com/haskell/cabal/issues/1883#issuecomment-44150139

# Initiate the build environment and build the executable.
#
# IMPORTANT: This must produce a statically-compiled binary (with respect to
# Cabal dependencies) that does not depend on a local cabal installation. The
# production Docker image will not run a cabal install.
RUN cabal update \
    && cabal sandbox init \
    && cabal sandbox add-source /root/build/submodule/atlassian-connect-descriptor \
    && cabal install --force-reinstalls

# Setup the default command to run for the container.
CMD ["/root/build/.cabal-sandbox/bin/confluence-pandoc-connect", "--access-log=-", "--error-log=stderr"]
