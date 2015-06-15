# The production docker file for the Remind Me Connect Haskell project.
# It is designed to be minimal by requiring only the dependencies of the production executable.
# It is also designed to be easy to maintain by being as short as possible.

FROM ubuntu:14.04
MAINTAINER Avi Knoll <aknoll@atlassian.com>

# Expose the default port required by Micros
EXPOSE 8080

# Install missing packages
USER root
RUN apt-get update && apt-get install -y libpq5 libgmp10 openjdk-7-jre-headless

# Copy our context into the build directory and start working from there
USER root
ADD /to-copy   /service

# Setup the Haskell environment
WORKDIR /service
ENV LANG en_US.UTF-8

# Run the main service
CMD ["/service/confluence-pandoc-connect", "--access-log=-", "--error-log=stderr", "--port=8080"]
