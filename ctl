#!/bin/sh
# -*- tab-width:4;indent-tabs-mode:nil -*-
# ex: ts=4 sw=4 et

CALLER_DIR=`pwd -P`
USE_DIR=$CALLER_DIR
SCRIPT_DIR=$(cd ${0%/*} && pwd)

NODE="exodm@127.0.0.1"

if [ -f ".exodm_env" ]; then
    . .exodm_env
else
# Emulate readlink -f to resolve the full physical directory
    cd `dirname $SCRIPT_DIR`
    TARGET_DIR=`basename $SCRIPT_DIR`

# Iterate down a (possible) chain of symlinks
    while [ -L "$TARGET_DIR" ]
    do
        TARGET_FILE=`readlink $TARGET_DIR`
        cd `dirname $TARGET_DIR`
        TARGET_FILE=`basename $TARGET_DIR`
    done

# Restore
    cd $CALLER_DIR

## Now set RUNNER_SCRIPT_DIR with symbolic links resolved
    RUNNER_SCRIPT_DIR=$CALLER_DIR/$TARGET_DIR

    if [ -f "$RUNNER_SCRIPT_DIR/bin/exodm.cmd" ]; then
    # ctl called in its original location
        d=$RUNNER_SCRIPT_DIR
        for i in `seq 1 3`;
        do
            d=`dirname $d`
        done
        EXODM_DIR=$d
        EXODM_RELEASE_DIR=$RUNNER_SCRIPT_DIR
#        EXODM="$RUNNER_SCRIPT_DIR/bin/exodm"
    else
        EXODM_DIR=$RUNNER_SCRIPT_DIR
        lnk=`readlink $EXODM_DIR/rel/exodm`
        EXODM_RELEASE_DIR=$EXODM_DIR/rel/$lnk
        echo "EXODM_DIR=$EXODM_DIR; EXODM_RELEASE_DIR=$EXODM_RELEASE_DIR"
#        EXODM="$EXODM_DIR/rel/exodm/bin/exodm"
    fi

    USE_DIR=$EXODM_DIR
fi

EXODM="$EXODM_RELEASE_DIR/bin/exodm"
echo "EXODM_DIR=$EXODM_DIR"
echo "EXODM_RELEASE_DIR=$EXODM_RELEASE_DIR"
echo "EXODM=$EXODM"

if [ -z "$ERL_SETUP_LIBS" ]; then
    EXODM="env ERL_SETUP_LIBS=\"$EXODM_DIR/rel/plugins\" $EXODM"
fi

while [ $# -gt 0 ]; do
    case "$1" in
        -n)
            shift
            USE_DIR=$EXODM_DIR/nodes/$1
            NODE="$1"
            shift
            ;;
        -l|-local)
            USE_DIR=$CALLER_DIR
            shift
            ;;
        start|attach|console|stop)
            shift
            CMD="$EXODM $1"
            echo "USE_DIR=$USE_DIR"
            echo "CMD = $CMD"
            cd $USE_DIR
            exec $CMD
            ;;
        convert)
            shift
            CMD="$EXODM console_boot exodm_setup -- -setup mode convert $*"
            while [ $# -gt 0 ]; do shift; done
            echo "CMD = $CMD"
            shift
            cd $USE_DIR
            exec $CMD
            ;;
        install)
            shift
            args=$*
            while [ $# -gt 0 ]; do shift; done
            ESCRIPT=$EXODM_RELEASE_DIR/erts/bin/escript
            if [ ! -d $USE_DIR ] ; then mkdir -p $USE_DIR ; fi
            VSN=`echo "$EXODM_RELEASE_DIR" | sed 's/.*\/exodm_\(.*\)$/\1/'`
            echo "VSN=$VSN"
            rd="$EXODM_RELEASE_DIR"
#            $ESCRIPT $rd/exorel current $VSN
            $ESCRIPT $rd/make_node -target $USE_DIR -rel $rd -- $args
            ;;
        rel)
            shift
            args=$*
            while [ $# -gt 0 ]; do shift; done
            ESCRIPT=$EXODM_RELEASE_DIR/erts/bin/escript
            cd $USE_DIR
            $ESCRIPT $EXODM_RELEASE_DIR/exorel $args
            ;;
        *)
            echo $"Usage: $0 [-l | -local] {start|attach|stop|convert|make_current|rel [args]}"
            exit 1
    esac
done

#cd $USE_DIR
#$CMD
