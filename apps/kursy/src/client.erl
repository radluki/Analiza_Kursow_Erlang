-module(client).

-export([register_user/2,depose/2,withdraw/2,buy/4,sell/3,get_balance/1,
	get_balance/2,get_autotraders/1,get_current_price/1,get_price/2,
	set_autosell_MACD/4,set_autobuy_MACD/5,	remove_autosell_MACD/2,
	remove_autobuy_MACD/2]).

register_user(Username,Password) ->
	serwer_kursow:delete_user(Username),	
	serwer_kursow:register_user(Username,Password).

depose(Username,Amount) ->
	serwer_kursow:depose(Username,Amount).

withdraw(Username,Amount) ->
	serwer_kursow:withdraw(Username,Amount).

buy(Username,Password,Code,Amount) ->
	serwer_kursow:buy(Username,Password,Code,Amount).

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

set_autosell_MACD(Username,Code,Amount,MinPrice) ->
	serwer_kursow:set_autosell_MACD(Username,Code,Amount,MinPrice).

set_autobuy_MACD(Username,Password,Code,Amount,MaxPrice) ->
	serwer_kursow:set_autobuy_MACD(Username,Password,Code,Amount,MaxPrice).

remove_autosell_MACD(Username,Code) ->
	serwer_kursow:remove_autosell_MACD(Username,Code).

remove_autobuy_MACD(Username,Code) ->
	serwer_kursow:remove_autobuy_MACD(Username,Code).
