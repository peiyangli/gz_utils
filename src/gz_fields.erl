%%%-------------------------------------------------------------------
%%% @author pei
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 五月 2019 9:27
%%%-------------------------------------------------------------------
-module(gz_fields).
-author("pei").

%% API
-export([converter_rune/1, converter_rune_binary/1, converter_jsonmap/0, converter_jsonmap/1]).
-export([converter_id/0, converter_integer/0, converter_integer/1, converter_float/0, converter_binary/0, converter_binary/1, converter_float/1]).

-export([source_property/1]).
-export([fields/2, fields/3]).

%%%-------------------------------------------------------------------
fields(Fields, Source)->
  fields(Fields, Source, #{}).

fields([], _, Acc)->
  {ok, Acc};
fields([F|Fields], Source, Acc) ->
  case convert(F, Source, Acc) of
    {ok, Acc1}->
      fields(Fields, Source, Acc1);
    Err->Err
  end.

%% {ok, NV}|{acc, NewAcc}|skip
call_converter(C, _F, V, _Acc) when is_function(C, 1)->
  C(V);
call_converter(C, F, V, _Acc) when is_function(C, 2)->
  C(F,V);
call_converter(C, F, V, Acc) when is_function(C, 3)->
  C(F,V,Acc).
%%query_call_converter(_, F, _, _)->
%%  {error, F}.


optional({optional, Default}, F, Acc)->
  {ok, Acc#{F => Default}};
optional(optional, _F, Acc)->
  {ok, Acc};
optional(_, F, _Acc)->
  {error, F}.

convert({F, Opt, C}, Source, Acc)->
  NewVal =
    case Source(F,undefined) of
      undefined -> undefined;
      V -> call_converter(C,F,V,Acc)
    end,
  case NewVal of
    {ok, Nv} -> {ok, Acc#{F => Nv}};
    {acc, Acc1} -> {ok, Acc1};
    skip -> {ok, Acc};
    Why->
      case optional(Opt, F, Acc) of
        {error, F} -> {error, F, Why};
        OK->OK
      end
  end.

%%%-------------------------------------------------------------------
source_property(Properties)->
  fun(F, Def)->
    proplists:get_value(F, Properties, Def)
  end.


%%%-------------------------------------------------------------------
ok_range(V, Min, Max) when Max < Min ->
  ok_range(V, Max, Min);
ok_range(V, Min, Max) ->
  if
    V < Min -> error;
    V > Max -> error;
    true-> ok
  end.

ok_min(V, Min)when Min > V ->error;
ok_min(_, _) -> ok.

ok_max(V, Max)when Max < V ->error;
ok_max(_, _) -> ok.

%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
converter_id()->
  fun(V)-> {ok, V} end.

%%%-------------------------------------------------------------------
converter_integer()->
  fun
    (V)when is_integer(V)->{ok, V};
    (V)->
    case string:to_integer(V) of
      {error, Why}-> {error, Why};
      {NV, _} -> {ok, NV}
    end
  end.

convert_ok_integer(V, OK) when is_integer(V) -> OK(V);
convert_ok_integer(V, OK) ->
  case string:to_integer(V) of
    {error, Why}-> {error, Why};
    {NV, _} -> OK(NV)
  end.

ok_integer(F)when is_function(F, 1)-> F;
ok_integer({clip, {A, B}})->
  fun(V)->
    {ok, dg_util:clip(V, A, B)}
  end;
ok_integer({range, {A, B}})->
  fun(V)->
    {ok_range(V, A, B), V}
  end;
ok_integer({min, A}) when is_integer(A)->
  fun(V)->
    {ok_min(V, A), V}
  end;
ok_integer({max, A}) when is_integer(A)->
  fun(V)->
    {ok_max(V, A), V}
  end;
ok_integer(F)->
  fun(_V)-> {error, {not_supported, F}} end.

% select a ok_integer function to check the result
% M: function(V)->{ok, V}
converter_integer(M)-> fun (V)-> convert_ok_integer(V, ok_integer(M)) end.

%%%-------------------------------------------------------------------

converter_float()->
  fun(V) when is_float(V)->{ok, V};
    (V)->
    case string:to_float(V) of
      {error, Why}-> {error, Why};
      {NV, _} -> {ok, NV}
    end
  end.

convert_ok_float(V, OK) when is_float(V) -> OK(V);
convert_ok_float(V, OK) ->
  case string:to_float(V) of
    {error, Why}-> {error, Why};
    {NV, _} -> OK(NV)
  end.

ok_float(F)when is_function(F, 1)-> F;
ok_float({clip, {A, B}})->
  fun(V)->
    {ok, dg_util:clip(V, A, B)}
  end;
ok_float({range, {A, B}})->
  fun(V)->
    {ok_range(V, A, B), V}
  end;
ok_float({min, A}) when is_float(A)->
  fun(V)->
    {ok_min(V, A), V}
  end;
ok_float({max, A}) when is_float(A)->
  fun(V)->
    {ok_max(V, A), V}
  end;
ok_float(F)->
  fun(_V)-> {error, {not_supported, F}} end.

% select a ok_integer function to check the result
converter_float(M) when is_function(M) -> fun (V)-> convert_ok_float(V, M) end;
converter_float(M)-> fun (V)-> convert_ok_float(V, ok_float(M)) end.
%%%-------------------------------------------------------------------
convert_binary(V) when is_binary(V)-> {ok, V};
convert_binary(V) when is_list(V)-> {ok, list_to_binary(V)};
convert_binary(V) when is_integer(V)-> {ok, integer_to_binary(V)};
convert_binary(V) when is_float(V)-> {ok, float_to_binary(V)};
convert_binary(V)when is_atom(V)-> {ok, list_to_binary(atom_to_list(V))};
convert_binary(_)->{error, not_supported}.



converter_binary()->
  fun(V) ->
    convert_binary(V)
  end.


convert_ok_binary(V, OK) ->
  case convert_binary(V) of
    {ok, Bin}-> OK(Bin);
    Err -> Err
  end.

ok_binary(F)when is_function(F, 1)-> F;
ok_binary({range, {A, B}})->
  fun(V)->
    {ok_range(byte_size(V), A, B), V}
  end;
ok_binary({min, A}) when is_integer(A)->
  fun(V)->
    {ok_min(byte_size(V), A), V}
  end;
ok_binary({max, A}) when is_integer(A)->
  fun(V)->
    {ok_max(byte_size(V), A), V}
  end;
ok_binary(F)->
  fun(_V)-> {error, {not_supported, F}} end.

converter_binary(M)-> fun (V)-> convert_ok_binary(V, ok_binary(M)) end.


%%%-------------------------------------------------------------------

convert_ok_rune(V, OK) ->
  case convert_binary(V) of
    {ok, Bin}->
      case unicode:characters_to_list(Bin) of
        List when is_list(List)-> OK(List);
        {error, List, _}-> OK(List);
        {incomplete, List, _} -> OK(List);
        Err -> Err
      end;
    Err -> Err
  end.


ok_rune(F)when is_function(F, 1)-> F;
ok_rune({range, {A, B}})->
  fun(V)->
    {ok_range(length(V), A, B), V}
  end;
ok_rune({min, A}) when is_integer(A)->
  fun(V)->
    {ok_min(length(V), A), V}
  end;
ok_rune({max, A}) when is_integer(A)->
  fun(V)->
    {ok_max(length(V), A), V}
  end;
ok_rune(F)->
  fun(_V)-> {error, {not_supported, F}} end.

converter_rune(M)-> fun (V)-> convert_ok_rune(V, ok_rune(M)) end.
%%  //utf8 length

%%%-------------------------------------------------------------------
rune_size(Bin)->
  case unicode:characters_to_list(Bin) of
    List when is_list(List)-> length(List);
    _X-> -1
  end.

ok_rune_binary(F)when is_function(F, 1)-> F;
ok_rune_binary({range, {A, B}})->
  fun(V)->
    {ok_range(rune_size(V), A, B), V}
  end;
ok_rune_binary({min, A}) when is_integer(A)->
  fun(V)->
    {ok_min(rune_size(V), A), V}
  end;
ok_rune_binary({max, A}) when is_integer(A)->
  fun(V)->
    {ok_max(rune_size(V), A), V}
  end;
ok_rune_binary(F)->
  fun(_V)-> {error, {not_supported, F}} end.

converter_rune_binary(M)-> fun (V)-> convert_ok_binary(V, ok_rune_binary(M)) end.
%%%-------------------------------------------------------------------


converter_jsonmap()->
  fun dg_util:json_decode/1.

%% require jiffy
convert_ok_jsonmap(V, OK) ->
  case dg_util:json_decode(V) of
    {ok, Map}-> OK(Map);
    Err -> Err
  end.

ok_jsonmap(F)when is_function(F, 1)-> F;
ok_jsonmap(F)-> fun(_V)-> {error, {not_supported, F}} end.

converter_jsonmap(M)->fun (V)-> convert_ok_jsonmap(V, ok_jsonmap(M)) end.
%%%-------------------------------------------------------------------