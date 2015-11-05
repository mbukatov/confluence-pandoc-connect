#!/bin/sh

set -e
set -u

USER="confluence_pandoc_connect"
DATABASE="confluence_pandoc_connect"

psql -U confluence_pandoc_connect confluence_pandoc_connect -a -f sql/V1__create_atlassian_connect.sql
