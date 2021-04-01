#!/bin/bash
export PORT=8080
export HMAC_KEY=/mnt/c/users/wbarden/personalprojects/wbws/secrets/hmac-key
export SEC_SALT=/mnt/c/users/wbarden/personalprojects/wbws/secrets/sec-salt
export DB_HOST=localhost
export DB_USERNAME=postgres
export DB_PASSWORD=password
export DB_DATABASE=general
export DB_TIMEOUT=4000
rebar3 shell
