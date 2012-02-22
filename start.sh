#!/bin/sh
cd `dirname $0`

export ERL_LIBS=lib

exec erl -pa $PWD/ebin -boot start_sasl -s errdb_web
