-module(mock_client).

-export([init/1,loop/2]).

-define(LOOP_NO,5).
-define(TIMEOUT_MOCK,500).

init(Username) ->
	client:register_user(Username),
	loop(Username,?LOOP_NO).

loop(Username,0) ->
	client:get_balance(Username);

loop(Username,N) ->
	client:depose(10000),
	%client:set_autobuy_MACD(Username,'CHF',1000,client:get_current_price('CHF')),
	client:buy(Username,'USD',rand:uniform(2000)),
	client:buy(Username,'EUR',rand:uniform(2000)),
	client:withdraw(Username,2000),
	client:withdraw(Username,200),
    timer:sleep(?TIMEOUT_MOCK),
	%client:set_autosell_MACD(Username,'USD',client:get_balance(Username,'USD'),client:get_current_price('USD')),
    client:sell(Username,'EUR',client:get_balance(Username,'EUR')),
    client:sell(Username,'CHF',client:get_balance(Username,'CHF')),
	loop(Username,N-1).