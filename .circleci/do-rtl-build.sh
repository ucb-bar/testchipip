#!/bin/bash

#-------------------------------------------------------------
# create the different verilator builds of testchipip
# the command is the make command string
#
# run location: circle ci docker image
# usage:
#   $1 - config string
#-------------------------------------------------------------

# turn echo on and error on earliest command
set -ex

# get shared variables
SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

rm -rf $LOCAL_CHIPYARD_DIR/generators/testchipip/*
mv -f $LOCAL_CHECKOUT_DIR/* $LOCAL_CHIPYARD_DIR/generators/testchipip/

TOOLS_DIR=$LOCAL_RISCV_DIR
LD_LIB_DIR=$LOCAL_RISCV_DIR/lib

# enter the verilator directory and build the specific config
export RISCV=$TOOLS_DIR
export LD_LIBRARY_PATH=$LD_LIB_DIR
export PATH=$LOCAL_VERILATOR_DIR/bin:$PATH
export VERILATOR_ROOT=$LOCAL_VERILATOR_DIR
export COURSIER_CACHE=$LOCAL_WORK_DIR/.coursier-cache
make -C $LOCAL_SIM_DIR clean
make -j$LOCAL_MAKE_NPROC -C $LOCAL_SIM_DIR JAVA_OPTS="$LOCAL_JAVA_OPTS" SBT_OPTS="$LOCAL_SBT_OPTS" ${mapping[$1]}
