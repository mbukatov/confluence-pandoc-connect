# Confluence Pandoc Connect

Import files into [Confluence](https://www.atlassian.com/software/confluence)
with the power of [Pandoc](http://pandoc.org/).

This is an [Atlassian Connect](http://connect.atlassian.com/) add-on written in
[Haskell](http://www.haskell.org).

## Issue tracking

See the [Confluence Pandoc Connect Kanban board](https://ecosystem.atlassian.net/secure/RapidBoard.jspa?rapidView=231).

# Running locally for development

## Dependencies

You'll need the following installed:

- The Haskell Platform, see `confluence-pandoc-connect.cabal` for minimum versions
- libpq development headers
- PostgreSQL

## Run

Set up a sandbox and install dependencies:

    make setup

Set up a postgres database:

    cd database
    sh bootstrap.sh
    sh init-db.sh

Run with some local values:

    CONNECT_BASE_URL="http://localhost:8001" \
    PG_CONFLUENCE_PANDOC_CONNECT_URL="postgres://confluence_pandoc_connect@localhost:5432/confluence_pandoc_connect" \
    cabal run -- --port 8001 --error-log=- --access-log=-


# Publishing a new version

## Sequence to build and push docker image

    bash build-production.bash
    bash save-docker-image.bash
    bash load-and-push-docker.bash

Ensure that the correct environment variables are set for load-and-push-docker

## Sequence to release new version

    micros service:deploy -v -e ddev -f service-descriptor.json confluence-pandoc-connect
