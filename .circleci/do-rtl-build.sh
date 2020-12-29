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

# call clean on exit
trap clean EXIT

# set stricthostkeychecking to no (must happen before rsync)
run "echo \"Ping $SERVER\""

clean

# copy over riscv/esp-tools, and chipyard to remote
run "mkdir -p $REMOTE_CHIPYARD_DIR"
copy $LOCAL_CHIPYARD_DIR/ $SERVER:$REMOTE_CHIPYARD_DIR

run "cp -r ~/.ivy2 $REMOTE_WORK_DIR"
run "cp -r ~/.sbt  $REMOTE_WORK_DIR"

TOOLS_DIR=$REMOTE_RISCV_DIR
LD_LIB_DIR=$REMOTE_RISCV_DIR/lib
run "mkdir -p $REMOTE_RISCV_DIR"
copy $LOCAL_RISCV_DIR/ $SERVER:$REMOTE_RISCV_DIR

# enter the verilator directory and build the specific config on remote server
run "export RISCV=\"$TOOLS_DIR\"; \
     export LD_LIBRARY_PATH=\"$LD_LIB_DIR\"; \
     export PATH=\"$REMOTE_VERILATOR_DIR/bin:\$PATH\"; \
     export VERILATOR_ROOT=\"$REMOTE_VERILATOR_DIR\"; \
     export COURSIER_CACHE=\"$REMOTE_WORK_DIR/.coursier-cache\"; \
     make -C $REMOTE_SIM_DIR clean; \
     make -j$REMOTE_MAKE_NPROC -C $REMOTE_SIM_DIR JAVA_OPTS=\"$REMOTE_JAVA_OPTS\" SBT_OPTS=\"$REMOTE_SBT_OPTS\" ${mapping[$1]}"
run "rm -rf $REMOTE_CHIPYARD_DIR/project"

# copy back the final build
mkdir -p $LOCAL_CHIPYARD_DIR
copy $SERVER:$REMOTE_CHIPYARD_DIR/ $LOCAL_CHIPYARD_DIR
