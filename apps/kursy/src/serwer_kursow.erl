-module(serwer_kursow).

-behaviour(gen_server).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([get_state/0,
	get_current_currency/1,
	get_currency/2]).

-ifdef(TEST).
-export([dziel_przez_0/0]).
-endif.

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

find_code(_,Dict) when is_atom(Dict) ->
    Dict;
find_code(Code,Dict) ->
    case maps:find(Code,Dict) of
        {ok,Val} -> Val;
	error -> not_found
    end.


handle_call({get,Date,Code},_,State) ->
    Data_Date = read_parsed_data(Date),
    {reply, find_code(Code,Data_Date), State};
handle_call({current,_},_,#state{current_data=error_404}=State) ->
    {reply,no_data_available,State};
handle_call(get_state,_,State) ->
    {reply,State,State};
handle_call({current,Code},_,State = #state{current_data=Dict}) ->
    {reply,find_code(Code,Dict),State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(Msg, State) ->
    handle_unrecognized(Msg),
    {noreply, State}.

handle_info(next_day, State) ->
    Date = State#state.timestamp,
    Data_tup = nbp:download_and_parse(Date),
    Data = convert_tuples_to_map(Data_tup),
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

get_current_currency(Code) ->
    gen_server:call(?MODULE, {current,Code}).

get_state() ->
    gen_server:call(?MODULE, get_state).

get_currency(Date={Y,M,D},Code) ->
    gen_server:call(?MODULE, {get,Date,Code}).

next_day(State,Data) ->
   D = calendar:date_to_gregorian_days(State#state.timestamp) + 1,
   State#state{timestamp = calendar:gregorian_days_to_date(D),
               current_data = Data}.

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

save_parsed_data(Date,Data) ->
    File = ?VAR_DIR ++ nbp:date_to_string(Date) ++ ".xml.parsed",
    nbp:save_data_txt(File++".txt",Data),
    nbp:save_data_bin(File,Data).

read_parsed_data(Date) -> 
    File = ?VAR_DIR ++ nbp:date_to_string(Date) ++ ".xml.parsed",
    case filelib:is_file(File) of
       true -> 
    	    Data = nbp:read_data_bin(File);
       false ->
            data_not_available
    end.

save_state(State) ->
    nbp:save_data_bin(?STATE_FILE,State).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

get_currency_test() ->
    start_link(),
    Date = {2004,5,5},
    Data_read = read_parsed_data(Date),
    Vals = maps:values(Data_read),
    Keys = maps:keys(Data_read),
    Vals2 = lists:map(fun(X) -> get_currency(Date,X) end, Keys),
    [] = Vals -- Vals2,
    data_not_available = get_currency({1999,1,1},"EUR"),
    not_found = get_currency(Date,"XXX").

read_parsed_data_test() ->
    Data = { parsed, data },
    Date = { 2017,12,1 },
    save_parsed_data(Date,Data),
    Data = read_parsed_data(Date).

handle_unrecognized(div_by_0) ->
    1/0.

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

save_state_test() ->
    {D,_} = x_test_some_current_data(),
    S = next_day(#state{},D),
    save_state(S),
    S = load_state().

dziel_przez_0() ->
    gen_server:cast(?MODULE,div_by_0).

-else.

handle_unrecognized(_) ->
    null.

-endif.
