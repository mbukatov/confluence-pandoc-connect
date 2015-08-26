#!/bin/sh -e

# See https://developer.atlassian.com/static/connect/docs/latest/guides/confluence-gardener-tutorial.html

atlas-run-standalone --product confluence --version 5.9.1-OD-65-062 --bundled-plugins com.atlassian.bundles:json-schema-validator-atlassian-bundle:1.0.4,com.atlassian.webhooks:atlassian-webhooks-plugin:1.0.6,com.atlassian.jwt:jwt-plugin:1.2.2,com.atlassian.upm:atlassian-universal-plugin-manager-plugin:2.19.1.2-D20150723T232127,com.atlassian.plugins:atlassian-connect-plugin:1.1.46 --jvmargs -Datlassian.upm.on.demand=true
