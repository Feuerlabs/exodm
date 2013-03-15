#!/bin/sh
# -*- tab-width:4;indent-tabs-mode:nil -*-
# ex: ts=4 sw=4 et

if [ -z $EXODM_DIR ]; then
    echo "EXODM_DIR env variable not set"
    exit 1
fi
CALLER_DIR=`pwd -P`
USE_DIR=$CALLER_DIR
SCRIPT_DIR=$(cd ${0%/*} && pwd)

NODE="exodm@127.0.0.1"

if [ -f ".exodm_env" ]; then
    . $PWD/.exodm_env
else
    if [ "$CALLER_DIR" == "$SCRIPT_DIR" ]; then
        lnk=`readlink $EXODM_DIR/rel/exodm`
        EXODM_RELEASE_DIR=$EXODM_DIR/rel/$lnk
    else
        BASE=`basename $SCRIPT_DIR`
        EXODM_RELEASE_DIR=$EXODM_DIR/rel/lib/$BASE
    fi
fi

EXODM="$EXODM_RELEASE_DIR/bin/exodm"

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
            CMD="$EXODM $1"
            shift
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
            echo $"Usage: $0 [-l | -local] {start|attach|stop|convert|install|rel [args]}"
            exit 1
    esac
done
