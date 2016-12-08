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

- [Stack](https://docs.haskellstack.org/en/stable/README/)
- libpq development headers
- PostgreSQL

## Build and run

1. Build the project:

    stack build

2. Set up a postgres database:

    cd migrations 
    sh bootstrap.sh
    sh init-db.sh

3. Setup a development environment as per the [Connect instructions](https://developer.atlassian.com/static/connect/docs/latest/developing/developing-locally.html). Use `PORT=8001`.

4. Set your ngrok URL in the env:

   export NGROK_URL="https://00000.ngrok.io"

4. Run with some local values, including your ngrok URL:

    stack build && \
    CONNECT_BASE_URL="${NGROK_URL}" \
    PG_CONFLUENCE_PANDOC_CONNECT_URL="postgres://confluence_pandoc_connect@localhost:5432/confluence_pandoc_connect" \
    stack exec confluence-pandoc-connect -- --port 8001 --error-log=- --access-log=-


# Publishing a new version

## Sequence to build and push docker image

    bash build-production.bash
    bash save-docker-image.bash
    bash load-and-push-docker.bash

Ensure that the correct environment variables are set for load-and-push-docker

## Sequence to release new version

    micros service:deploy -v -e ddev -f service-descriptor.json confluence-pandoc-connect

# Micros

## Environment variables

The values `CONNECT_SECRET_KEY` and `CONNECT_BASE_URL` are set in Micros environments that we deploy to using `micros stash:set`.
