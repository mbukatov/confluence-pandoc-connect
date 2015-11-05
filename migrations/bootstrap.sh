#!/bin/sh

set -e
set -u

if [ "$#" -ne 1 ]; then
  echo "Usage $0 IP"
  exit -1
fi

HOST="$1"

USER="confluence_pandoc_connect"
DATABASE="confluence_pandoc_connect"

# Does the user exist?
if [ ! $(psql postgres -tAc "SELECT 1 FROM pg_roles WHERE rolname='$USER'") ]; then
    createuser $USER
fi

# Does the database exist?
DB_EXISTS=$(psql -lqt | cut -d \| -f 1 | grep -w ${DATABASE} | wc -l | tr -d ' ')
if [[ "$DB_EXISTS" -ne "1" ]]; then
    createdb -O $USER $DATABASE
fi
