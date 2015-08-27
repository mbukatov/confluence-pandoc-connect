# Confluence Pandoc Connect

Import files into [Confluence](https://www.atlassian.com/software/confluence)
with the power of [Pandoc](http://pandoc.org/).

This is an [Atlassian Connect](http://connect.atlassian.com/) add-on written in
[Haskell](http://www.haskell.org).

# Publishing a new version

## Sequence to build and push docker image

    bash build-production.bash
    bash save-docker-image.bash
    bash load-and-push-docker.bash

Ensure that the correct environment variables are set for load-and-push-docker

## Sequence to release new version

    micros service:deploy -v -e ddev -f service-descriptor.json confluence-pandoc-connect
