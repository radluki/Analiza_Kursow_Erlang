-module(client_callbacks_tests).

-include_lib("eunit/include/eunit.hrl").

register_test() ->
	Username = test1,
	Password = xxx,
	serwer_kursow:register_user(Username,Password),
	already_registered = serwer_kursow:register_user(Username,Password).

depose_no_user_test() ->
	Username = test21,
	no_user = serwer_kursow:depose(user,1000).

depose_test() ->
	Username = test2,
	Password = xyz,
	Status = serwer_kursow:register_user(Username,Password),
	case Status of
		{ok} ->
			0 = serwer_kursow:get_balance(Username,"PLN"),
			{ok} = serwer_kursow:depose(Username,1000),
			1000 = serwer_kursow:get_balance(Username,"PLN");
		_Else ->
			true
	end.

withdraw_test() ->
	Username = test3,
	Password = xyz,
	serwer_kursow:register_user(Username,Password),
	serwer_kursow:depose(Username,1000),
	{ok} = serwer_kursow:withdraw(Username,1000),
	0 = serwer_kursow:get_balance(Username,"PLN").

withdraw_no_user_test() ->
	Username = test31,
	no_user = serwer_kursow:withdraw(Username,10000).

withdraw_not_enough_test() ->
	Username = test32,
	Password = xyz,
	serwer_kursow:register_user(Username,Password),
	serwer_kursow:depose(Username,1000),
	not_enough_money = serwer_kursow:withdraw(Username,10000).

buy_test() ->
	Username = test4,
	Password = xyz,
	Code = "CHF",
	Price = serwer_kursow:get_current_price(Code),
    if
    	(Price == null) -> buy_test();
    	true -> 
			no_user = serwer_kursow:buy(Username,Password,Code,10000),
			% authentication_failed = serwer_kursow:buy(Username,Password,Code,10000),
			serwer_kursow:register_user(Username,Password),
			serwer_kursow:depose(Username,100),
			not_enough_money = serwer_kursow:buy(Username,Password,Code,10000),
			{ok} = serwer_kursow:buy(Username,Password,Code,1),
			1 = serwer_kursow:get_balance(Username,Code)
	end.

sell_test() ->
	Username = test5,
	Password = qqq,
	Code = "CHF",
	Price = serwer_kursow:get_current_price(Code),
    if
    	(Price == null) -> sell_test();
    	true -> 
			no_user = serwer_kursow:sell(Username,Code,10000),
			serwer_kursow:register_user(Username,Password),
			serwer_kursow:depose(Username,1000),
			not_enough_money = serwer_kursow:sell(Username,Code,10000),
			{ok} = serwer_kursow:buy(Username,Password,Code,1),
			1 = serwer_kursow:get_balance(Username,Code),
			{ok} = serwer_kursow:sell(Username,Code,1),
			0 = serwer_kursow:get_balance(Username,Code)
	end.

get_balance_test() ->
	Username = test6,
	Password = qwe,
	Code = "CHF",
	Price = serwer_kursow:get_current_price(Code),
    if
    	(Price == null) -> get_balance_test();
    	true -> 
			no_user = serwer_kursow:get_balance(Username,Code),
			no_user = serwer_kursow:get_balance(Username),
			serwer_kursow:register_user(Username,Password),
			serwer_kursow:depose(Username,1000),
			0 = serwer_kursow:get_balance(Username,Code),
			M1 = #{"PLN" => 1000},
			M1 = serwer_kursow:get_balance(Username),
			serwer_kursow:buy(Username,Password,Code,1),
			1 = serwer_kursow:get_balance(Username,Code)
	end.

get_autotraders_test() ->
	Username = test7,
	Password = xyz,
	no_user = serwer_kursow:get_autotraders(Username),
	serwer_kursow:register_user(Username,Password),
	serwer_kursow:set_autosell_MACD(Username,"CHF",1000,4.0),
	true = maps:is_key("sell_CHF_MACD",serwer_kursow:get_autotraders(Username)),
	serwer_kursow:remove_autosell_MACD(Username,"CHF"),
	false = maps:is_key("sell_CHF_MACD",serwer_kursow:get_autotraders(Username)),
	serwer_kursow:set_autobuy_MACD(Username,Password,"CHF",1000,4.0),
	true = maps:is_key("buy_CHF_MACD",serwer_kursow:get_autotraders(Username)),
	serwer_kursow:remove_autobuy_MACD(Username,"CHF"),
	false = maps:is_key("buy_CHF_MACD",serwer_kursow:get_autotraders(Username)).
