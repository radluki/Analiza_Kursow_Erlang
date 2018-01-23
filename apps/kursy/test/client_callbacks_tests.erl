-module(client_callbacks_tests).

-include_lib("eunit/include/eunit.hrl").

register_test() ->
	Username = test1,
	serwer_kursow:register_user(Username),
	already_registered = serwer_kursow:register_user(Username).

depose_test() ->
	Username = test2,
	no_user = serwer_kursow:depose(user,1000),
	serwer_kursow:register_user(Username),
	{ok} = serwer_kursow:depose(Username,1000),
	1000 = serwer_kursow:get_balance(Username,"PLN").

withdraw_test() ->
	Username = test3,
	no_user = serwer_kursow:withdraw(Username,10000),
	serwer_kursow:register_user(Username),
	serwer_kursow:depose(Username,1000),
	not_enough_money = serwer_kursow:withdraw(Username,10000),
	{ok} = serwer_kursow:withdraw(Username,1000),
	0 = serwer_kursow:get_balance(Username,"PLN").

buy_test() ->
	Username = test4,
	Code = "CHF",
	Price = serwer_kursow:get_current_price(Code),
    if
    	(Price == null) -> buy_test();
    	true -> 
			no_user = serwer_kursow:buy(Username,Code,10000),
			serwer_kursow:register_user(Username),
			serwer_kursow:depose(Username,100),
			not_enough_money = serwer_kursow:buy(Username,Code,10000),
			{ok} = serwer_kursow:buy(Username,Code,1),
			1 = serwer_kursow:get_balance(Username,Code)
	end.

sell_test() ->
	Username = test5,
	Code = "CHF",
	Price = serwer_kursow:get_current_price(Code),
    if
    	(Price == null) -> sell_test();
    	true -> 
			no_user = serwer_kursow:sell(Username,Code,10000),
			serwer_kursow:register_user(Username),
			serwer_kursow:depose(Username,1000),
			not_enough_money = serwer_kursow:sell(Username,Code,10000),
			{ok} = serwer_kursow:buy(Username,Code,1),
			1 = serwer_kursow:get_balance(Username,Code),
			{ok} = serwer_kursow:sell(Username,Code,1),
			0 = serwer_kursow:get_balance(Username,Code)
	end.

get_balance_test() ->
	Username = test6,
	Code = "CHF",
	Price = serwer_kursow:get_current_price(Code),
    if
    	(Price == null) -> get_balance_test();
    	true -> 
			no_user = serwer_kursow:get_balance(Username,Code),
			no_user = serwer_kursow:get_balance(Username),
			serwer_kursow:register_user(Username),
			serwer_kursow:depose(Username,1000),
			0 = serwer_kursow:get_balance(Username,Code),
			M1 = #{"PLN" => 1000},
			M1 = serwer_kursow:get_balance(Username),
			serwer_kursow:buy(Username,Code,1),
			1 = serwer_kursow:get_balance(Username,Code)
	end.

% get_autotraders_test() ->
% 	serwer_kursow:get_autotraders(Username).