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

-export([get_current_currency/1]).

% Skipped 2 years of data, because format change:
-define(INIT_DATE,{2004,5,3}).
-define(TIMEOUT,5000).
-define(VAR_DIR,".conf/").
-define(STATE_FILE, ?VAR_DIR ++ "state.bin").
-record(state,{timestamp = ?INIT_DATE, current_data=no_current_data }).

start_link() ->
    create_var_dir(),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    State = load_state(),
    erlang:send_after(?TIMEOUT,self(),next_day),
    {ok, State,?TIMEOUT}.

get_current_currency(Code) ->
    gen_server:call(?MODULE, {current,Code}).

handle_call({current,_},From,#state{current_data=error_404}=State) ->
    {reply,no_current_data,State};
handle_call({current,Code},From,State = #state{current_data=CD}) ->
    case maps:find(Code,CD) of
        {ok,Val} -> {reply, Val, State};
	error -> {reply, not_found, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(next_day, State) ->
    Date = {Y,M,D} = State#state.timestamp,
    io:format("New day: ~p-~p-~p~n",[Y,M,D]),
    Data = nbp:download_and_parse(Date),
    State_new = next_day(State,Data),
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

next_day(State,Data) ->
   D = calendar:date_to_gregorian_days(State#state.timestamp) + 1,
   State#state{timestamp = calendar:gregorian_days_to_date(D),
               current_data = convert_tuples_to_map(Data)}.

create_var_dir() ->
    case filelib:is_dir(?VAR_DIR) of
        false -> file:make_dir(?VAR_DIR);
        true -> null
    end.

load_state() ->
    case filelib:is_file(?STATE_FILE) of
        true -> nbp:read_data_bin(?STATE_FILE);
        false -> #state{current_data=not_available}
    end.

convert_tuples_to_map(State_tuples) when is_list(State_tuples)->
    lists:foldl( fun({_,Code,Price},Acc) -> 
                     Acc#{Code => Price} 
		end, #{}, State_tuples );
convert_tuples_to_map(X) -> X.

x_test_some_current_data() ->
    N1 = "Abc", N2 = "Bcd", V1 = 12, V2 = 30,
    State_tuples = [ {yy,N1,V1}, {xx,N2,V2} ],
    M = #{N1 => V1, N2 => V2},
    {State_tuples, M}.

convert_tuples_to_map_test() ->
    {State_tuples,M} = x_test_some_current_data(),
    M3 = convert_tuples_to_map(State_tuples),
    nbp:save_data_txt("tmp/map_of_state.txt",M3),
    M = M3.

save_parsed_data(Date,Data) ->
    File = ?VAR_DIR ++ nbp:date_to_string(Date) ++ ".xml.parsed",
    nbp:save_data_txt(File++".txt",Data),
    nbp:save_data_bin(File,Data).

save_state(State) ->
    nbp:save_data_bin(?STATE_FILE,State).

save_state_test() ->
    {D,_} = x_test_some_current_data(),
    S = next_day(#state{},D),
    save_state(S),
    S = load_state().


