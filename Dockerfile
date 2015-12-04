# The development docker file for the Confluence Pandoc Connect Haskell project.
# This docker file is designed to help us build the production executables but
# should not be used to actually generate the production docker images. Instead
# we should make a new production image that takes the executables from this
# image and only includes them. That way we do not need to carry the entire
# Haskell platform with us into production. Just the small set of required
# dependencies.

FROM fpco/stack-build
MAINTAINER Avi Knoll <aknoll@atlassian.com>

# Expose the default port, port 8000
EXPOSE 8000

# Copy our context into the build directory and start working from there
ADD .   /root/build

# Setup the Haskell Envoronment
WORKDIR /root/build
ENV LANG en_US.UTF-8 # See: https://github.com/haskell/cabal/issues/1883#issuecomment-44150139

# Initiate the build environment and build the executable.
RUN stack build

# Setup the default command to run for the container.
CMD ["stack", "exec", "confluence-pandoc-connect", "--", "--access-log=-", "--error-log=stderr"]
