#!/bin/sh
# -*- tab-width:4;indent-tabs-mode:nil -*-
# ex: ts=4 sw=4 et

CALLER_DIR=$PWD

SCRIPT_DIR=$(cd ${0%/*} && pwd)

## Emulate readlink -f to resolve the full physical directory
cd `dirname $SCRIPT_DIR`
TARGET_DIR=`basename $SCRIPT_DIR`

# Iterate down a (possible) chain of symlinks
while [ -L "$TARGET_DIR" ]
do
    TARGET_FILE=`readlink $TARGET_DIR`
    cd `dirname $TARGET_DIR`
    TARGET_FILE=`basename $TARGET_DIR`
done

# Compute the canonicalized name by finding the physical path
# for the directory we're in and appending the target file.
PHYS_DIR=`pwd -P`

# Restore
cd $CALLER_DIR

## Now set RUNNER_SCRIPT_DIR with symbolic links resolved
RUNNER_SCRIPT_DIR=$PHYS_DIR/$TARGET_DIR

if [ -f "$RUNNER_SCRIPT_DIR/bin/exodm.cmd" ]; then
    echo "ctl called in original location"
    # ctl called in its original location
    d=$RUNNER_SCRIPT_DIR
    for i in `seq 1 3`;
    do
        d=`dirname $d`
    done
    EXODM_DIR=$d
    EXODM="$RUNNER_SCRIPT_DIR/bin/exodm"
else
    echo "assume ctl copied to exodm dir"
    EXODM_DIR=$RUNNER_SCRIPT_DIR
    EXODM="$EXODM_DIR/rel/exodm/bin/exodm"
fi

echo "EXODM_DIR=$EXODM_DIR"

USE_DIR=$RUNNER_SCRIPT_DIR

if [ -z "$ERL_SETUP_LIBS" ]; then
    EXODM="ERL_SETUP_LIBS=\"$RUNNER_SCRIPT_DIR/rel/plugins\" $EXODM"
fi

while [ $# -gt 0 ]; do
    case "$1" in
        -l|-local)
            USE_DIR=$CALLER_DIR
            shift
            ;;
        start|attach|console|stop)
            CMD="$EXODM $1"
            shift
            ;;
        *)
            echo $"Usage: $0 [-l | -local] {start|attach|stop|convert}"
            exit 1

    esac
done

echo "CMD = $CMD"
echo "USE_DIR = $USE_DIR"
cd $USE_DIR
env $CMD
