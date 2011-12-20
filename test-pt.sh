# !/bin/sh

export ERL_LIBS=..\

erlc -I include/ test/victim.erl
