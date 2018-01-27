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
	get_current_price/1,
	get_price/2,
	get_price_list/3,
	register_user/2,
	depose/2,
	withdraw/2,
	buy/4,
	sell/3,
	get_balance/1,
	get_balance/2,
	get_autotraders/1,
	set_autosell_MACD/4,
	set_autobuy_MACD/5,
	remove_autosell_MACD/2,
	remove_autobuy_MACD/2,

	monitor_init/8,
	autosell_MACD/4,
	autobuy_MACD/5]).

-ifdef(TEST).
-export([dziel_przez_0/0]).
-endif.

% Skipped 2 years of data, because format change:
-define(INIT_DATE,{2004,5,3}).
-define(TIMEOUT,1000).
-define(VAR_DIR,".conf/").
-define(STATE_FILE, ?VAR_DIR ++ "state.bin").
%%State
-record(state,{timestamp = ?INIT_DATE, current_data=null, users=#{}}).

start_link() ->
    create_var_dir(),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    State = load_state(),
    erlang:send_after(?TIMEOUT,self(),next_day),
    {ok, State,?TIMEOUT}.

find_code(_,Dict) when is_atom(Dict) ->
    null;
find_code(Code,Dict) ->
    case maps:find(Code,Dict) of
        {ok,Val} -> Val;
	error -> null
    end.


handle_call({get,InitD,List_of_days,Code},_,State) ->
    List_of_prices = lists:map(
    		     fun(X) -> 
		     	Dict = read_parsed_data(date_plus_days(InitD,X)),
			find_code(Code,Dict)
		     end, List_of_days),
    {reply, List_of_prices, State};
handle_call({get,Date,Code},_,State) ->
    Data_Date = read_parsed_data(Date),
    {reply, find_code(Code,Data_Date), State};
handle_call({current,_},_,#state{current_data=error_404}=State) ->
    {reply,null,State};
handle_call(get_state,_,State) ->
    {reply,State,State};
handle_call({current,Code},_,State = #state{current_data=Dict}) ->
    {reply,find_code(Code,Dict),State};
% NEW HANDLERS
handle_call({register_user,Username,Password},_,State = #state{users=Users}) ->
	case maps:is_key(Username,Users) of
        false -> 
            cpp_port:register_user(Username,Password),
            {reply,{ok},
            	State#state{users = Users#{Username => {#{"PLN" => 0},#{}}}}};
        true -> 
            {reply,already_registered,State}
    end;

handle_call({depose,Username,Amount},_,State = #state{users=Users}) ->
	case maps:is_key(Username,Users) of
        true -> 
        	{Money,Autotraders} = maps:get(Username,Users),
			Money_new = Money#{"PLN" => (maps:get("PLN",Money) + Amount)},
			{reply,{ok},State#state{users=Users#{Username => {Money_new,Autotraders}}}};
        false -> 
            {reply,no_user,State}
    end;

handle_call({withdraw,Username,Amount},_,State = #state{users=Users}) ->
	case maps:is_key(Username,Users) of
        true -> 
        	{Money,Autotraders} = maps:get(Username,Users),
        	case maps:get("PLN",Money) >= Amount of
        		true -> 
					Money_new = Money#{"PLN" => (maps:get("PLN",Money) - Amount)},
					{reply,{ok},
						State#state{users=Users#{Username => {Money_new,Autotraders}}}};
        		false -> 
            		{reply,not_enough_money,State}
    		end;
        false -> 
            {reply,no_user,State}
    end;

handle_call({buy,Username,Password,Code,Amount},_,State = #state{current_data=Dict,users=Users}) ->

    case maps:is_key(Username,Users) of
        true -> 
            case cpp_port:check_password(Username,Password) of
                ok ->
                    Price = find_code(Code,Dict),
        			case Price of
                		null -> 
                    		{reply,no_current_price,State};
                		_Else -> 
        		        	{Money,Autotraders} = maps:get(Username,Users),
        		        	case (maps:get("PLN",Money,0) >= (Amount*Price)) of
        		        		true -> 
        		        			Money_new = Money#{Code => (maps:get(Code,Money,0) + Amount),
        									"PLN" => (maps:get("PLN",Money,0) - Amount*Price)},
        							io:format(user,"~p bought ~p ~p~n",[Username,Amount,Code]),
        							{reply,{ok},
        								State#state{users=Users#{Username => {Money_new,Autotraders}}}};
        		        		false -> 
        		            		{reply,not_enough_money,State}
        		    		end
            		end;
                _E -> {reply,authentication_failed,State}
            end;
        false -> 
            {reply,no_user,State}
    end;

handle_call({sell,Username,Code,Amount},_,State = #state{current_data=Dict,users=Users}) ->
	case maps:is_key(Username,Users) of
        true -> 
        	{Money,Autotraders} = maps:get(Username,Users),
        	case maps:get(Code,Money,0) >= Amount of
        		true -> 
        			Price = find_code(Code,Dict),
        			case Price of
		        		null -> 
		            		{reply,no_current_price,State};
		        		_Else -> 
							Money_new = Money#{Code => (maps:get(Code,Money,0) - Amount),
											"PLN" => (maps:get("PLN",Money,0) + Amount*Price)},
							%io:format(user,"~p sold ~p ~p~n",[Username,Amount,Code]),
							{reply,{ok},
								State#state{users=Users#{Username => {Money_new,Autotraders}}}}
		    		end;
        		false -> 
            		{reply,not_enough_money,State}
    		end;
        false -> 
            {reply,no_user,State}
    end;

handle_call({get_balance,Username},_,State = #state{users=Users}) ->
	case maps:is_key(Username,Users) of
        true -> 
        	{Money,_Autotraders} = maps:get(Username,Users),
            {reply,Money,State};
        false -> 
            {reply,no_user,State}
    end;

handle_call({get_balance,Username,Code},_,State = #state{users=Users}) ->
	case maps:is_key(Username,Users) of
        true -> 
        	{Money,_Autotraders} = maps:get(Username,Users),
            {reply,maps:get(Code,Money,0),State};
        false -> 
            {reply,no_user,State}
    end;

handle_call({get_autotraders,Username},_,State = #state{users=Users}) ->
	case maps:is_key(Username,Users) of
        true -> 
        	{_Money,Autotraders} = maps:get(Username,Users),
            {reply,Autotraders,State};
        false -> 
            {reply,no_user,State}
    end;

handle_call({set_autosell_MACD,Username,Code,Amount,MinPrice},_,State = #state{users=Users}) ->
	case maps:is_key(Username,Users) of
        true -> 
        	{Money,Autotraders} = maps:get(Username,Users),
        	Key = "sell_" ++ Code ++ "_MACD",
        	case maps:is_key(Key,Autotraders) of
        		true ->
            		{reply,already_exists,State};
            	false ->
        			Pid = spawn(?MODULE,autosell_MACD,[Username, Code, Amount, MinPrice]),
        			Monitor_pid = spawn(?MODULE,monitor_init,
        				[Pid, Username,"", Code, Amount, MinPrice, "sell", "MACD"]),
        			Autotraders_new = Autotraders#{Key => {Pid,Monitor_pid}},
        			{reply,{ok},State#state{users=Users#{Username => {Money,Autotraders_new}}}}
        	end;
        false -> 
            {reply,no_user,State}
    end;

handle_call({set_autobuy_MACD,Username,Password,Code,Amount,MaxPrice},_,State = #state{users=Users}) ->
	case maps:is_key(Username,Users) of
        true -> 
        	{Money,Autotraders} = maps:get(Username,Users),
        	Key = "buy_" ++ Code ++ "_MACD",
        	case maps:is_key(Key,Autotraders) of
        		true ->
            		{reply,already_exists,State};
            	false ->
        			Pid = spawn(?MODULE,autobuy_MACD,[Username, Password, Code, Amount, MaxPrice]),
        			Monitor_pid = spawn(?MODULE,monitor_init,
        				[Pid, Username, Password, Code, Amount, MaxPrice, "buy", "MACD"]),
        			Autotraders_new = Autotraders#{Key => {Pid,Monitor_pid}},
        			{reply,{ok},State#state{users=Users#{Username => {Money,Autotraders_new}}}}
        	end;
        false -> 
            {reply,no_user,State}
    end;

handle_call({remove_autosell_MACD,Username,Code},_,State = #state{users=Users}) ->
	case maps:is_key(Username,Users) of
        true -> 
        	{Money,Autotraders} = maps:get(Username,Users),
        	Key = "sell_" ++ Code ++ "_MACD",
        	case maps:is_key(Key,Autotraders) of
        		true ->
					{Pid,Monitor_pid} = maps:get(Key,Autotraders),
        			erlang:exit(Monitor_pid,kill),
        			erlang:exit(Pid,kill),
        			Autotraders_new = maps:remove(Key,Autotraders),
        			{reply,{ok},State#state{users=Users#{Username => {Money,Autotraders_new}}}};
            	false ->
            		{reply,no_trader,State}
            end;
        false -> 
            {reply,no_user,State}
    end;

handle_call({remove_autobuy_MACD,Username,Code},_,State = #state{users=Users}) ->
    case maps:is_key(Username,Users) of
        true -> 
        	{Money,Autotraders} = maps:get(Username,Users),
        	Key = "buy_" ++ Code ++ "_MACD",
        	case maps:is_key(Key,Autotraders) of
        		true ->
					{Pid,Monitor_pid} = maps:get(Key,Autotraders),
        			erlang:exit(Monitor_pid,kill),
        			erlang:exit(Pid,kill),
        			Autotraders_new = maps:remove(Key,Autotraders),
        			{reply,{ok},State#state{users=Users#{Username => {Money,Autotraders_new}}}};
            	false ->
            		{reply,no_trader,State}
            end;
        false -> 
            {reply,no_user,State}
    end;

% END
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({update_pid,Username,Key,Pid}, State = #state{users=Users}) ->
	case maps:is_key(Username,Users) of
        true -> 
        	{Money,Autotraders} = maps:get(Username,Users),
        	case maps:is_key(Key,Autotraders) of
        		true ->
        			{_,Monitor_pid} = maps:get(Key,Autotraders),
        			Autotraders_new = Autotraders#{Key => {Pid,Monitor_pid}},
        			{noreply,State#state{users=Users#{Username => {Money,Autotraders_new}}}};
        		false ->
        			{noreply,State}
        	end;
        false -> 
            {noreply,State}
    end;

handle_cast(Msg, State) ->
    handle_unrecognized(Msg),
    {noreply, State}.

handle_info(next_day, State) ->
    Date = date_plus_days(State#state.timestamp,1),
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

% CALLBACKS
get_price_list(Init_date,Days_from_list,Code) ->
    gen_server:call(?MODULE,{get,Init_date,Days_from_list,Code}).

get_current_price(Code) ->
    gen_server:call(?MODULE, {current,Code}).

get_state() ->
    gen_server:call(?MODULE, get_state).

get_price(Date={_Y,_M,_D},Code) ->
    gen_server:call(?MODULE, {get,Date,Code}).

% NEW CALLBACKS

register_user(Username,Password) ->
	gen_server:call(?MODULE,{register_user,Username,Password}).

depose(Username,Amount) ->
	gen_server:call(?MODULE,{depose,Username,Amount}).

withdraw(Username,Amount) ->
	gen_server:call(?MODULE,{withdraw,Username,Amount}).

buy(Username,Password,Code,Amount) ->
	gen_server:call(?MODULE,{buy,Username,Password,Code,Amount}).

sell(Username,Code,Amount) ->
	gen_server:call(?MODULE,{sell,Username,Code,Amount}).

get_balance(Username) ->
	gen_server:call(?MODULE,{get_balance,Username}).

get_balance(Username,Code) ->
	gen_server:call(?MODULE,{get_balance,Username,Code}).

get_autotraders(Username) ->
	gen_server:call(?MODULE,{get_autotraders,Username}).

set_autosell_MACD(Username,Code,Amount,MinPrice) ->
	gen_server:call(?MODULE,{set_autosell_MACD,Username,Code,Amount,MinPrice}).

set_autobuy_MACD(Username,Password,Code,Amount,MaxPrice) ->
	gen_server:call(?MODULE,{set_autobuy_MACD,Username,Password,Code,Amount,MaxPrice}).

remove_autosell_MACD(Username,Code) ->
	gen_server:call(?MODULE,{remove_autosell_MACD,Username,Code}).

remove_autobuy_MACD(Username,Code) ->
	gen_server:call(?MODULE,{remove_autobuy_MACD,Username,Code}).

% CALLBACKS END

date_plus_days(Date,Days) ->  
   D = calendar:date_to_gregorian_days(Date) + Days,
   calendar:gregorian_days_to_date(D).

%%State
next_day(State,Data) ->
   D = date_plus_days(State#state.timestamp,1),
   State#state{timestamp = D,
               current_data = Data}.

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
    	    nbp:read_data_bin(File);
       false ->
            null
    end.

save_state(State) ->
    nbp:save_data_bin(?STATE_FILE,State).

monitor_init(Pid,Username,Password,Code,Amount,Price,Mode,Algorithm) ->
           erlang:monitor(process,Pid),
           monitor_loop(Username,Password,Code,Amount,Price,Mode,Algorithm).

monitor_loop(Username,Password,Code,Amount,Price,Mode,Algorithm) -> 
  receive
    {'DOWN',_,_,_,_} -> 
        Key = Mode ++ "_" ++ Code ++ "_" ++ Algorithm,
        case Mode of
        	"sell" -> 
        		Pid = spawn(?MODULE,autosell_MACD,[Username, Code, Amount, Price]),
    			gen_server:cast(?MODULE,{update_pid,Username,Key,Pid});
        	"buy" -> 
        		Pid = spawn(?MODULE,autobuy_MACD,[Username, Password, Code, Amount, Price]),
    			gen_server:cast(?MODULE,{update_pid,Username,Key,Pid});
        	true -> null
        end
  end.

autosell_MACD(Username,Code,Amount,MinPrice) ->
	Price = get_current_price(Code),
	case Price of
		null ->
    		timer:sleep(?TIMEOUT),
			autosell_MACD(Username,Code,Amount,MinPrice);
		_Else ->
			EMA12 = Price,
			EMA26 = Price,
		    timer:sleep(?TIMEOUT),
			autosell_MACD(Username,Code,Amount,MinPrice,EMA12,EMA26)
	end.

autosell_MACD(Username,Code,Amount,MinPrice,EMA12,EMA26) ->
    Price = get_current_price(Code),
	case Price of
		null ->
    		timer:sleep(?TIMEOUT),
			autosell_MACD(Username,Code,Amount,MinPrice,EMA12,EMA26);
		_Else ->
			EMA12_new = (Price*2+EMA12*11)/13,
			EMA26_new = (Price*2+EMA26*25)/27,
			MACD = EMA12-EMA26,
			MACD_new = EMA12_new-EMA26_new,
			%io:format("MACD autosell ~p, ~p, EMA12: ~f, EMA26: ~f~n",[Username,Code,EMA12_new,EMA26_new]),
			if
				((MACD_new<0) and (MACD>0) and (Price>MinPrice)) ->
					Sell_result = sell(Username,Code,Amount),
					case Sell_result of
						{ok} ->
							remove_autosell_MACD(Username,Code);
						_Else ->
		    				timer:sleep(?TIMEOUT),
							autosell_MACD(Username,Code,Amount,MinPrice,EMA12_new,EMA26_new)
					end;
				true ->		
		    		timer:sleep(?TIMEOUT),			
					autosell_MACD(Username,Code,Amount,MinPrice,EMA12_new,EMA26_new)
			end
	end.

autobuy_MACD(Username,Password,Code,Amount,MaxPrice) ->
	Price = get_current_price(Code),
	case Price of
		null ->
    		timer:sleep(?TIMEOUT),
			autobuy_MACD(Username,Password,Code,Amount,MaxPrice);
		_Else ->
			EMA12 = Price,
			EMA26 = Price,
		    timer:sleep(?TIMEOUT),
			autobuy_MACD(Username,Password,Code,Amount,MaxPrice,EMA12,EMA26)
	end.

autobuy_MACD(Username,Password,Code,Amount,MaxPrice,EMA12,EMA26) ->
    Price = get_current_price(Code),
	case Price of
		null ->
    		timer:sleep(?TIMEOUT),
			autobuy_MACD(Username,Password,Code,Amount,MaxPrice,EMA12,EMA26);
		_Else ->
			EMA12_new = (Price*2+EMA12*11)/13,
			EMA26_new = (Price*2+EMA26*25)/27,
			MACD = EMA12-EMA26,
			MACD_new = EMA12_new-EMA26_new,
			%io:format("MACD autobuy ~p, ~p, EMA12: ~f, EMA26: ~f~n",[Username,Code,EMA12_new,EMA26_new]),
			if
				((MACD_new>0) and (MACD<0) and (Price<MaxPrice)) ->
					Buy_result = buy(Username,Password,Code,Amount),
					case Buy_result of
						{ok} ->
							remove_autobuy_MACD(Username,Code);
						_Else ->
		    				timer:sleep(?TIMEOUT),
							autobuy_MACD(Username,Password,Code,Amount,MaxPrice,EMA12_new,EMA26_new)
					end;
				true ->
		    		timer:sleep(?TIMEOUT),
					autobuy_MACD(Username,Password,Code,Amount,MaxPrice,EMA12_new,EMA26_new)
			end
	end.

%======================================
%% Tests
%======================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

get_price_test() ->
    start_link(),
    Date = {2004,5,5},
    Data_tup = nbp:download_and_parse(Date),
    Data_read = convert_tuples_to_map(Data_tup),
    save_parsed_data(Date,Data_read),
    Vals = maps:values(Data_read),
    Keys = maps:keys(Data_read),
    Vals2 = lists:map(fun(X) -> get_price(Date,X) end, Keys),
    [] = Vals -- Vals2,
    null = get_price({1999,1,1},"EUR"),
    null = get_price(Date,"XXX").

read_parsed_data_test() ->
    Data = { parsed, data },
    Date = { 2017,12,1 },
    save_parsed_data(Date,Data),
    Data = read_parsed_data(Date).

handle_unrecognized(div_by_0) ->
    1/0;
handle_unrecognized(_) ->
    null.

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

%%State
save_state_test() ->
    {_,D} = x_test_some_current_data(),
    S = next_day(#state{},D),
    save_state(S),
    S = load_state().

dziel_przez_0() ->
    gen_server:cast(?MODULE,div_by_0).

-else.

handle_unrecognized(_) ->
    null.

-endif.
