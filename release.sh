#!/bin/bash

INSTALL_PATH=$1
INSTALL_CMD="target_system:install(\"redis\", \"$INSTALL_PATH\"), init:stop()."

erl -pa lib/redis-1.0/ebin/ -eval 'target_system:create("redis"), init:stop().'
#echo $INSTALL_CMD
erl -pa lib/redis-1.0/ebin/ -eval "$INSTALL_CMD"

