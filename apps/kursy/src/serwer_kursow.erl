-module(serwer_kursow).

-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

% Skipped 2 years of data, because format change:
-define(INIT_DATE,{2004,5,3}).
-define(TIMEOUT,5000).
-define(VAR_DIR,".conf/").
-define(STATE_FILE, ?VAR_DIR ++ "state.bin").
-record(state,{timestamp = ?INIT_DATE }).

create_var_dir() ->
    case filelib:is_dir(?VAR_DIR) of
        false -> file:make_dir(?VAR_DIR);
        true -> null
    end.

load_state() ->
    case filelib:is_file(?STATE_FILE) of
        true -> nbp:read_data_bin(?STATE_FILE);
        false -> #state{}
    end.

start_link() ->
    create_var_dir(),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    State = load_state(),
    erlang:send_after(?TIMEOUT,self(),next_day),
    {ok, State,?TIMEOUT}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

save_parsed_data(Date,Data) ->
    File = ?VAR_DIR ++ nbp:date_to_string(Date) ++ ".xml.parsed",
    nbp:save_data_txt(File++".txt",Data),
    nbp:save_data_bin(File,Data).

save_state(State) ->
    nbp:save_data_bin(?STATE_FILE,State).

save_state_test() ->
    S = next_day(#state{}),
    save_state(S),
    S = load_state().

handle_info(next_day, State) ->
    State_new = next_day(State),
    Date = {Y,M,D} = State#state.timestamp,
    io:format("New day: ~p-~p-~p~n",[Y,M,D]),
    Data = nbp:download_and_parse(Date),
    case Data of
     error_404 -> null;
     Data -> save_parsed_data(Date,Data)
    end, 
    save_state(State_new),
    erlang:send_after(?TIMEOUT, self(), next_day),
    {noreply, State_new};
handle_info(_,State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%======================================
%% Implementation
%======================================

next_day(State) ->
   D = calendar:date_to_gregorian_days(State#state.timestamp) + 1,
   State#state{timestamp = calendar:gregorian_days_to_date(D)}.


