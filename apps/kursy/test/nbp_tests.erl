-module(nbp_tests).

-include_lib("eunit/include/eunit.hrl").

save_data_txt_test() ->
   File = "./tmp/test.txt",
   Data = #{testowe=>22, dane=>12},
   nbp:save_data_txt(File,Data),
   Content = nbp:read_data_txt(File),
   Content = lists:flatten(io_lib:format("~p",[Data])).

save_data_bin_test() ->
   File = "./tmp/test.bin",
   Data = #{testowe=>22, dane=>12},
   nbp:save_data_bin(File,Data),
   Data = nbp:read_data_bin(File). 

read_and_parse_test() ->
   File = "./test_data/nbp_2018-01-02.xml",
   Downloaded = nbp:download_and_parse({2018,1,2}),
   Content = nbp:read_and_parse(File),
   Downloaded = Content.

download_and_parse_nonexistent_test() ->
   error_404 = nbp:download_and_parse({2018,1,1}),
   error_404 = nbp:download_and_parse({2004,5,3}).

t2004_test() ->
   nbp:download_and_parse({2004,5,4}).

t2018_test() -> 
   nbp:download_and_parse({2018,1,2}).
