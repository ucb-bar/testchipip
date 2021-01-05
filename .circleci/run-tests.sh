#!/bin/bash

#-------------------------------------------------------------
# run the different tests (based on the config)
#
# run location: circle ci docker image
# usage:
#   $1 - config string
#-------------------------------------------------------------

# turn echo on and error on earliest command
set -ex

# get remote exec variables
SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

case $1 in
    unittest)
        make BINARY="none" run-binary-fast -C $LOCAL_SIM_DIR ${mapping[$1]}
        ;;
    *)
        echo "No set of tests for $1. Did you spell it right?"
        exit 1
        ;;
esac
