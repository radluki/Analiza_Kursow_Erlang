-module(client).

-export([register_user/1,depose/2,withdraw/2,buy/3,sell/3,get_balance/1,
	get_balance/2,get_autotraders/1,set_autosell_MACD/4,set_autobuy_MACD/4,
	remove_autoseller_MACD/2,remove_autobuyer_MACD/2]).

register_user(Username) ->
	serwer_kursow:register_user(Username).

depose(Username,Amount) ->
	serwer_kursow:depose(Username,Amount).

withdraw(Username,Amount) ->
	serwer_kursow:withdraw(Username,Amount).

buy(Username,Code,Amount) ->
	serwer_kursow:buy(Username,Code,Amount).

sell(Username,Code,Amount) ->
	serwer_kursow:sell(Username,Code,Amount).

get_balance(Username) ->
	serwer_kursow:get_balance(Username).

get_balance(Username,Code) ->
	serwer_kursow:get_balance(Username,Code).

get_autotraders(Username) ->
	serwer_kursow:get_autotraders(Username).

get_current_price(Code) ->
	serwer_kursow:get_current_price(Code).

get_price(Date={_Y,_M,_D},Code) ->
	serwer_kursow:get_price(Date,Code).

%TODO
set_autosell_MACD(Username,Code,Amount,MinPrice) ->
	{ok}.

set_autobuy_MACD(Username,Code,Amount,MaxPrice) ->
	{ok}.

remove_autoseller_MACD(Username,Code) ->
	{ok}.

remove_autobuyer_MACD(Username,Code) ->
	{ok}.