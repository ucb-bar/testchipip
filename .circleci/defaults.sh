#!/bin/bash

# shared variables between the different services

#############
# SHARED VARS
#############

# make parallelism
CI_MAKE_NPROC=8
LOCAL_MAKE_NPROC=4

# verilator version
VERILATOR_VERSION=v4.034

# local variables (aka within the docker container)
LOCAL_WORK_DIR=$HOME
LOCAL_VERILATOR_DIR=$HOME/verilator-install
LOCAL_JAVA_OPTS="-Xmx2500M -Xss8M"
LOCAL_SBT_OPTS="-Dsbt.ivy.home=$LOCAL_WORK_DIR/.ivy2 -Dsbt.supershell=false -Dsbt.global.base=$LOCAL_WORK_DIR/.sbt -Dsbt.boot.directory=$LOCAL_WORK_DIR/.sbt/boot"
LOCAL_CHECKOUT_DIR=$HOME/project
LOCAL_RISCV_DIR=$HOME/riscv-tools-install
LOCAL_ESP_DIR=$HOME/esp-tools-install
LOCAL_CHIPYARD_DIR=$HOME/chipyard
LOCAL_SIM_DIR=$LOCAL_CHIPYARD_DIR/sims/verilator

# key value store to get the build strings
declare -A mapping
mapping["unittest"]="SUB_PROJECT=testchipip"
