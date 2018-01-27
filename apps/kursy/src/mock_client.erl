-module(mock_client).

-export([start/1,init/1,loop/3]).

-define(LOOP_NO,500).
-define(TIMEOUT_MOCK,5000).

start(Username) ->
    P = spawn_link(?MODULE,init,[Username]),
    register(?MODULE,P),
    {ok,P}.

init(Username) ->
	Password = qwew,
	client:register_user(Username,Password),
	serwer_kursow:remove_autosell_MACD(Username,"USD"),
	serwer_kursow:remove_autobuy_MACD(Username,"CHF"),
	loop(Username,Password,?LOOP_NO).

loop(Username,Password,0) ->
	client:get_balance(Username);

loop(Username,Password,N) ->
	client:depose(Username,10000),
	client:set_autobuy_MACD(Username,"CHF",1000,client:get_current_price("CHF")),
	client:buy(Username,Password,"USD",rand:uniform(2000)),
	client:buy(Username,Password,"EUR",rand:uniform(2000)),
	client:withdraw(Username,2000),
	client:withdraw(Username,200),
    timer:sleep(?TIMEOUT_MOCK),
	client:set_autosell_MACD(Username,"USD",client:get_balance(Username,"USD"),client:get_current_price("USD")),
    client:sell(Username,"EUR",client:get_balance(Username,"EUR")),
    client:sell(Username,"CHF",client:get_balance(Username,"CHF")),
	loop(Username,Password,N-1).
