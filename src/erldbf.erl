%%%-------------------------------------------------------------------
%%% @author Vitali Kletsko
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. мар 2015 15:34
%%%-------------------------------------------------------------------
-module(erldbf).
-author("Vitali Kletsko <v.kletsko@gmail.com>").

%%==================================
%% API
%%==================================
-export([parse/1, parse_bin/1, get_result_set/1]).

-export([start/0, stop/0]).

-include("erldbf.hrl").

start() ->
    F = fun({App, _, _}) -> App end,
    RunningApps = lists:map(F, application:which_applications()),
    LoadedApps = lists:map(F, application:loaded_applications()),
    case lists:member(?MODULE, LoadedApps) of
      true ->   true;
      false ->  ok = application:load(?MODULE)
    end,
    {ok, Apps} = application:get_key(?MODULE, applications),
    [ok = application:start(A) || A <- Apps ++ [?MODULE], not lists:member(A, RunningApps)],
    ok.
            
stop() ->
    application:stop(?MODULE).

-spec parse_bin(binary()) -> {ok, FileStructure}|{error, atom()}
  when FileStructure::#file_struct{}.
parse_bin(BinContent) when is_binary(BinContent) ->
  {ok, FileStructure} = decode_header(BinContent),
  parse_rows(FileStructure).

-spec parse(list()) -> {ok, FileStructure}|{error, atom()}
  when FileStructure::#file_struct{}.
parse(FileName) when is_list(FileName) ->
  case file:read_file(FileName) of
    {ok, Bin} ->
      io:format("Size BIn Content: ~p~n", [byte_size(Bin)]),
      {ok, FileStructure} = decode_header(Bin),
      parse_rows(FileStructure);
    Error -> Error
  end.

-spec get_result_set(FileStructure::#file_struct{}) -> {ok, list()}|{error, atom()}.
get_result_set(#file_struct{fields = Fields, rows = Rows}) ->
  try
    {ok, [lists:zip([Fname || #field_descriptor{field_name = Fname} <- Fields], Row) || Row <- Rows]}
  catch
    _:E -> E
  end.

-spec decode_header(binary()) -> {ok, list()}.
decode_header(
    <<Vsn/integer,
    %% DateTime
    Yy/integer, Mm/integer, Dd/integer,
    N:4/little-unit:8,   %% 8 bytes
    FieldsStripe:2/little-unit:8, RecordStripe:2/little-unit:8,   %% 4 bytes
    _Z:20/unit:8, Rest/binary>>) ->
    {FieldDescriptor, Rows} = split_binary(Rest, FieldsStripe - 33),
    Fields = field_descriptor(FieldDescriptor, []),
    {ok, #file_struct{
        vsn = Vsn,
        date = {Yy, Mm, Dd},
        record_stripe = RecordStripe,
        count_rows = N,
        fields = lists:reverse(Fields),
        fields_len = length(Fields),
        buffer = skip_rows(Rows)}
    }.


-spec skip_rows(binary()) -> binary().
skip_rows(<<" ", Rest/binary>>) ->
  skip_rows(Rest);
skip_rows(<<"\r", Rest/binary>>) ->
  skip_rows(Rest);
skip_rows(Rest) -> Rest.

-spec parse_rows(FileStructure::#file_struct{}) -> {ok, NewFileStructure::#file_struct{}}.
parse_rows(#file_struct{buffer = <<>>, rows = Acc} = S) -> {ok, S#file_struct{rows = lists:reverse(Acc)}};
parse_rows(#file_struct{record_stripe = RS, fields = Fields, buffer = Buffer, rows = Acc} = S) ->
  case byte_size(Buffer) >= RS of
    true ->
      case split_binary(Buffer, RS) of
        {Row, <<>>} ->
          parse_rows(S#file_struct{buffer = <<>>, rows = [parse_row(Fields, Row)|Acc]});
        %% Mark as Deleted
        {<<$*, _DeletedRow/binary>>, <<>>} ->
          parse_rows(S#file_struct{buffer = <<>>, rows = Acc});
        %% Mark as Deleted
        {<<$*, _DeletedRow/binary>>, ?ENDMARK} ->
          parse_rows(S#file_struct{buffer = <<>>, rows = Acc});
        %% Mark as Deleted
        {<<$*, _DeletedRow/binary>>, Rest} ->
          parse_rows(S#file_struct{buffer = Rest, rows = Acc});
        {Row, ?ENDMARK} ->
          parse_rows(S#file_struct{buffer = <<>>, rows = [parse_row(Fields, Row)|Acc]});
        {Row, Rest} ->
          parse_rows(S#file_struct{buffer = Rest, rows = [parse_row(Fields, Row)|Acc]})
      end;
    false ->
      parse_rows(S#file_struct{buffer = <<>>, rows = [parse_row(Fields, Buffer)|Acc]})
  end.

-spec parse_row(list(), binary(), integer()) -> {ok, list()}.
parse_row(L, RowBin) ->
  parse_row(L, skip_whitespace(RowBin), []).

parse_row([], _Rest, Acc) -> lists:reverse(Acc);
parse_row([#field_descriptor{field_type = <<"C">>, field_length = Length}|T], Row, Acc) ->
  case split_binary(Row, Length) of
    {Record, <<>>} ->
      parse_row(T, <<>>, [skip_pad(right, Record)|Acc]);
    {Record, Rest} ->
      parse_row(T, Rest, [skip_pad(right, Record)|Acc])
  end;
parse_row([#field_descriptor{field_type = <<"L">>}|T], Row, Acc) ->
  case split_binary(Row, 1) of
    {Record, <<>>} ->
      parse_row(T, <<>>, [Record|Acc]);
    {Record, Rest} ->
      parse_row(T, Rest, [Record|Acc])
  end;
parse_row([#field_descriptor{field_type = <<"N">>, field_length = Length}|T], Row, Acc) ->
  case split_binary(Row, Length) of
    {Record, <<>>} ->
      parse_row(T, <<>>, [skip_pad(left, Record)|Acc]);
    {Record, Rest} ->
      parse_row(T, Rest, [skip_pad(left, Record)|Acc])
  end;
parse_row([#field_descriptor{field_type = <<"F">>, field_length = Length}|T], Row, Acc) ->
  case split_binary(Row, Length) of
    {Record, <<>>} ->
      parse_row(T, <<>>, [Record|Acc]);
    {Record, Rest} ->
      parse_row(T, Rest, [Record|Acc])
  end;
parse_row([#field_descriptor{field_type = <<"D">>}|T], Row, Acc) ->
  case split_binary(Row, 8) of
    {Record, <<>>} ->
      parse_row(T, <<>>, [parse_date(Record)|Acc]);
    {Record, Rest} ->
      parse_row(T, Rest, [parse_date(Record)|Acc])
  end.

-spec skip_whitespace(binary()) -> binary().
skip_whitespace(<<" ", Row/binary>>) -> Row;
skip_whitespace(Row) -> Row.

-spec field_descriptor(binary(), list()) -> list().
field_descriptor(<<>>, Acc) -> Acc;
field_descriptor(Fields, Acc) ->
  case split_binary(Fields, 32) of
    {Field, Rest} ->
      field_descriptor(Rest, [parse_field_desc(Field)| Acc]);
    _Any -> Acc
  end.

-spec parse_field_desc(binary()) -> FileStructure::#file_struct{}.
parse_field_desc(<<FName:11/binary, FType:5/binary, FLength/integer, _Rest/binary>>) ->
  #field_descriptor{field_name = skip_zero(FName), field_type = detect_field_type(FType), field_length = FLength}.

-spec parse_date(binary()) -> erlang:date().
parse_date(<<Year:4/binary, Month:2/binary, Day:2/binary>>) ->
  {binary_to_integer(Year), binary_to_integer(Month), binary_to_integer(Day)}.

skip_pad(left_once, Data) ->
  lists:last(binary:split(Data, <<32>>));
skip_pad(left,Data) ->
  lists:last(binary:split(Data, <<32>>, [global]));
skip_pad(right,Data) ->
  join_str_val(binary:split(Data, <<"\s">>, [global, trim])).

-spec skip_zero(binary()) -> binary().
skip_zero(Bin) ->
  hd(binary:split(Bin, <<0>>, [trim])).

-spec detect_field_type(binary()) -> binary().
detect_field_type(<<FieldType:1/binary, _/binary>>) -> FieldType.

-spec join_str_val(list()) -> binary().
join_str_val(L) ->
  join_str_val(L, []).

join_str_val([], Acc) -> iolist_to_binary(lists:reverse(Acc));
join_str_val([H|[]], Acc) ->
  join_str_val([], [H|Acc]);
join_str_val([H|T], Acc) ->
  join_str_val(T, [[H, $\s]|Acc]).
