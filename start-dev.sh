#!/bin/sh
cd `dirname $0`

export ERL_LIBS=lib

"${MAKE}"
exec erl -pa $PWD/ebin -boot start_sasl -s reloader -s errdb_web
