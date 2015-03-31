%%%-------------------------------------------------------------------
%%% @author Vitali Kletsko
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. мар 2015 15:34
%%%-------------------------------------------------------------------
-module(erldbf_tests).
-author("Vitali Kletsko <v.kletsko@gmail.com").

-define(FileName, "sample_data.dbf").

-include("erldbf.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
basic_test_() ->
  [
    ?_assert(run() =:= {<<"999999">>, <<"Leistung">>})
  ].

run() ->
  {ok, FileStruct} = erldbf:parse(filename:join([code:priv_dir('erldbf'), ?FileName])),
  {ok, Rows} = erldbf:get_result_set(FileStruct),
  FirstRow = hd(Rows),
  {
    proplists:get_value(<<"LEISTNR">>, FirstRow),
    proplists:get_value(<<"LEISTUNG">>, FirstRow)
  }.

