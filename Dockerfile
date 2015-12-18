FROM fpco/stack-full
MAINTAINER Avi Knoll <aknoll@atlassian.com>

# Expose the default port, port 8000
EXPOSE 8000

# Copy our context into the build directory and start working from there
ADD .   /build
WORKDIR /build

# Initiate the build environment and build the executable.
RUN stack setup
RUN stack build
