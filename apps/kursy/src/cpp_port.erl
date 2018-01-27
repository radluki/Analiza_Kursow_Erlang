-module(cpp_port).
-export([start/0, start/1, stop/0, init/1]).
-export([register_user/2, check_password/2]).

start() ->
    start("./serwer_hasel").

start(ExtPrg) ->
    spawn(?MODULE, init, [ExtPrg]).
stop() ->
    complex ! stop.

register_user(Id,Password) ->
    call_port({register_user, Id, Password}).
check_password(Id,Password) ->
    call_port({check_password, Id, Password}).

call_port(Msg) ->
    complex ! {call, self(), Msg},
    receive
	{complex, Result} ->
	    Result
    end.

init(ExtPrg) ->
    register(complex, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg}, [{packet, 2}, binary]),
    loop(Port).

loop(Port) ->
    receive
	{call, Caller, Msg} ->
	    Port ! {self(), {command, term_to_binary(Msg)}},
	    receive
		{Port, {data, Data}} ->
		    Caller ! {complex, binary_to_term(Data)}
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
