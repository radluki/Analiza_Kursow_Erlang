%%%-------------------------------------------------------------------
%% @doc kursy top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(kursy_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    cpp_port:start(),
    timer:sleep(500),
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 3, period => 5},
    ChildSpecs = [#{id => serwer,
                    start => {serwer_kursow, start_link, []},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [serwer_kursow]},
                {mock,{mock_client, start, [client1]},permanent,
                    brutal_kill, worker,[mock_client]}],
    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

dziel_przez_0_test() ->
    start_link(),
    serwer_kursow:dziel_przez_0(),
    timer:sleep(200),
    serwer_kursow:get_state(). 

-endif.
