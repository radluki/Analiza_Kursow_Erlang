-module(complex1).
-export([start/1, stop/0, init/1]).
-export([register_user/2, check_password/2]).
-export([run/0]).

run() ->
    start("./serwer_hasel.py"),
    register_user(1,"xxx").


start(ExtPrg) ->
    spawn(?MODULE, init, [ExtPrg]).
stop() ->
    complex ! stop.

register_user(Id,Password) ->
    call_port(encode({register_user,Id,Password})).

check_password(Id,Password) ->
    call_port(encode({check_password,Id,Password})).

call_port(Msg) ->
    complex ! {call, self(), Msg},
    receive
	{complex, Result} ->
	    Result
    end.

init(ExtPrg) ->
    register(complex, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg}, [{packet, 2}]),
    loop(Port).

loop(Port) ->
    receive
	{call, Caller, Msg} ->
	    Port ! {self(), {command, encode(Msg)}},
	    receive
		{Port, {data, Data}} ->
		    Caller ! {complex, decode(Data)}
	    end,
	    loop(Port);
	stop ->
	    Port ! {self(), close},
	    receive
		{Port, closed} ->
		    exit(normal)
	    end;
	{'EXIT', Port, Reason} ->
	    exit(port_terminated)
    end.

encode({register_user,Id,Password}) ->
	list_to_tuple([1,Id] ++ Password);
encode({check_password,Id,Password}) ->
	list_to_tuple([2,Id] ++ Password).

decode(X) -> X.
