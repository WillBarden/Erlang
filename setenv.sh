#!/bin/bash

for env_file in $@
do
    if [ -f $env_file ]; then
        export $(grep -v '^#' $env_file | xargs)
    fi
done
