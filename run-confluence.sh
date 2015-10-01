#!/bin/sh -e

# See https://developer.atlassian.com/static/connect/docs/latest/guides/confluence-gardener-tutorial.html

atlas-run-standalone --product confluence --version 5.9.1-OD-2015.40.1-0001 --data-version 5.9.1-OD-2015.40.1-0001 --bundled-plugins com.atlassian.bundles:json-schema-validator-atlassian-bundle:1.0.4,com.atlassian.webhooks:atlassian-webhooks-plugin:1.0.6,com.atlassian.jwt:jwt-plugin:1.2.2,com.atlassian.upm:atlassian-universal-plugin-manager-plugin:2.20.1-D20150924T170115,com.atlassian.plugins:atlassian-connect-plugin:1.1.54 --jvmargs -Datlassian.upm.on.demand=true
