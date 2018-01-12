-module(nbp).

% xmlElement and xmlText records deffinitions:
-include_lib("xmerl/include/xmerl.hrl").


-export([date_to_string/1,
         read_and_parse/1,
         download_and_parse/1,
	 save_data_bin/2,
	 read_data_bin/1,
	 save_data_txt/2,
	 read_data_txt/1,
	 http_get_xml_data/1
	 ]).


-define(TABLE_A,"http://api.nbp.pl/api/exchangerates/tables/A/").
-define(xml(X),X++"?format=xml").
-define(error_404(X),"404 NotFound"==string:sub_string(X,1,12)).
   
date_to_string({Y,M,D}) ->
   io_lib:format("~p-~2..0B-~2..0B",[Y,M,D]).

http_get_xml_data(DD={_Y,_M,_D}) ->
   Date = date_to_string(DD),
   {ok,{_,_,Body}} = httpc:request(?xml(?TABLE_A ++ Date)),
   Body.

save_data_bin(File,Data) ->
   file:write_file(File,term_to_binary(Data)).

read_data_bin(File) ->
   {ok,Content} = file:read_file(File),
   binary_to_term(Content).

% PARSER
parse_list_of_rates(Data) ->
    C1 = Data#xmlElement.content,
    C11 = hd(C1),
    C2 = C11#xmlElement.content,
    C24 = lists:nth(4,C2),
    C24#xmlElement.content.

parse_rate(Rate) ->
    [Currency,Code,Price] = Rate#xmlElement.content,
    Curr_val = get_value(Currency),
    Code_val = get_value(Code),
    {Price_val,_} = string:to_float(get_value(Price)),
    {Curr_val,Code_val,Price_val}.
    
get_value(Element) ->
    C1 = hd(Element#xmlElement.content),
    C1#xmlText.value.

parse_rates_to_list_of_vals(Rates) ->
    lists:map(fun(X) -> parse_rate(X) end, Rates).
% END PARSER

save_data_txt(File,Data) ->
   {ok,D} = file:open(File,[write]),
   io:format(D,"~p",[Data]),
   file:close(D).

read_data_txt(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    try get_all_lines(Device)
      after file:close(Device)
    end.

get_all_lines(Device) ->
    case io:get_line(Device, "") of
        eof  -> [];
        Line -> Line ++ get_all_lines(Device)
    end.

download_and_parse(D={_,_,_}) ->
   Body = http_get_xml_data(D),
   parse_string(Body).

parse_string(Body) ->
 case  ?error_404(Body) of
  true -> error_404;
  false ->
   {Element,_} = xmerl_scan:string(Body),
   Rates = parse_list_of_rates(Element),
   parse_rates_to_list_of_vals(Rates)
 end.

read_and_parse(File) ->
   Body = read_data_txt(File),
   parse_string(Body).

tmp_file_name(Name) ->
    {_,_,Us} = erlang:timestamp(),
    {{Y,Mo,D},{H,Mi,S}} = calendar:local_time(),
    io_lib:format("./tmp/~s_~p-~p-~p_~p:~p:~p_~p",[Name,Y,Mo,D,H,Mi,S,Us]).


% TESTS ================================================

-ifdef(TEST).

% assert definitions and export of test functions:
-include_lib("eunit/include/eunit.hrl").
%
-define(Table_02,"./test_data/nbp_2018-01-02.xml").

parser_test() ->
   Body = http_get_xml_data({2018,1,2}),
   {Element,_} = xmerl_scan:string(Body),
   Rates = parse_list_of_rates(Element),
   R1 = hd(Rates),
   Vals = parse_rate(R1),
   Vals = {"bat (Tajlandia)","THB",0.1065}.

parser_whole_file_test() ->
   Data = read_data_txt("./test_data/nbp_2018-01-02.xml"),
   {Element,_} = xmerl_scan:string(Data),
   Rates = parse_list_of_rates(Element),
   Val_list = parse_rates_to_list_of_vals(Rates),
   File = "./tmp/parsed_nbp_2018-01-02.xml.parsed",
   save_data_txt(File,Val_list),
   save_data_bin(File,Val_list),
   Val_list = read_data_bin(File).

http_request_test() ->
   Body = http_get_xml_data({2018,1,2}),
   File = tmp_file_name("lukitest"),
   {ok,D} = file:open(File,[write]),
   io:format(D,"~s",[Body]),
   file:close(D),
   [] = os:cmd("diff "++File++" "++?Table_02),
   ok = file:delete(File).

tmp_file_name_collision_test() ->
   A = tmp_file_name(""),
   B = tmp_file_name(""),
   ?assert(A=/=B).

-endif.
