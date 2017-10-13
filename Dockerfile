FROM fpco/stack-build:lts-7
MAINTAINER Avi Knoll <aknoll@atlassian.com>

# Copy our context into the build directory and start working from there
ADD .   /build
WORKDIR /build

# Initiate the build environment and build the executable.
RUN stack setup
RUN stack build --only-dependencies
RUN stack build
