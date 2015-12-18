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
