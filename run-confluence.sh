#!/bin/sh -e

# See https://developer.atlassian.com/static/connect/docs/latest/guides/confluence-gardener-tutorial.html

atlas-run-standalone --container tomcat7x --product confluence --version 5.9.1-OD-2015.45.1-0006 --data-version 5.9.1-OD-2015.45.1-0006 --bundled-plugins com.atlassian.bundles:json-schema-validator-atlassian-bundle:1.0.4,com.atlassian.jwt:jwt-plugin:1.2.2,com.atlassian.upm:atlassian-universal-plugin-manager-plugin:2.20.1-D20150924T170115,com.atlassian.plugins:atlassian-connect-plugin:1.1.57 --jvmargs -Datlassian.upm.on.demand=true
