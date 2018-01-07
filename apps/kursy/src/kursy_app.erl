%%%-------------------------------------------------------------------
%% @doc kursy public API
%% @end
%%%-------------------------------------------------------------------

-module(kursy_app).

-behaviour(application).

-include_lib("eunit/include/eunit.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    application:start(inets),
    application:start(ssl),
    kursy_sup:start_link(),
    serwer_kursow:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

