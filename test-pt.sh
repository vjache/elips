# !/bin/sh

export ERL_LIBS=..\

erlc -I include/ test/victim_0.erl
