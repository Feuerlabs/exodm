#!/bin/sh
#  Send some json-rpc commands to an exosense server.
#
curl -u ga:wewontechcrunch2011 -k -X POST https://localhost:8088/ck3/rpc -d @$1