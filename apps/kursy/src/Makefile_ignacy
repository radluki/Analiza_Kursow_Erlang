OUT_PATH=../../../
ERL_INT=/usr/lib/erlang/lib/erl_interface-3.10.1
CC=g++
CFLAGS = -I $(ERL_INT)/include -L $(ERL_INT)/lib
OPTS = -lerl_interface -lei -lpthread

$(OUT_PATH)serwer_hasel: 
	$(CC) erl_comm.cpp ei.cpp $(CFLAGS) $(OPTS) -o $@

