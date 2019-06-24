%%%-------------------------------------------------------------------
%%% @author pei
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. 十一月 2018 20:53
%%%-------------------------------------------------------------------
-module(gz_bin).
-author("pei").

%% API
-export([replace/3, part/2, from/1, convert/1]).
%%-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
replace(<<>>, _, _) ->
  <<>>;
replace(Bin, Value, Range)when is_tuple(Range) ->
  DL = byte_size(Bin),
  case dg_util:normal_range(DL, Range) of
    {ok, {PosA, PosB}}->
      do_bin_replace_pos(Bin, DL, Value, PosA, PosB - PosA);
    _->Bin
  end;
replace(Bin, Value, Pos0) when Pos0 < 0 ->
  DL = byte_size(Bin),
  Pos = DL + Pos0 + 1,
  if
    Pos < 0 -> Bin;
    true-> do_bin_replace_pos(Bin, DL, Value, Pos, 1)
  end;
replace(Bin, Value, Pos0) ->
  DL = byte_size(Bin),
  if
    DL - Pos0 < 0 -> Bin;
    true-> do_bin_replace_pos(Bin, DL, Value, Pos0, 1)
  end.

do_bin_replace_pos(Bin, _, _, _, Len) when Len < 1 ->
  Bin;
do_bin_replace_pos(Bin, DL, _, Pos0, _) when DL =< Pos0 ->
  Bin;
do_bin_replace_pos(Bin, _, Value, Pos0, Len) when is_binary(Value) ->
  <<Prefix:Pos0/binary, _:Len/binary, Suffix/binary>> = Bin,
  <<Prefix:Pos0/binary, Value/binary, Suffix/binary>>;
do_bin_replace_pos(Bin, _, Value, Pos0, Len) ->
  <<Prefix:Pos0/binary, _:Len/binary, Suffix/binary>> = Bin,
  <<Prefix:Pos0/binary, Value, Suffix/binary>>.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


part(<<>>, _)->
  <<>>;
part(_, {P, P})->
  <<>>;
part(Bin, {PosMin, PosMax}) when PosMax < PosMin->
  part(Bin, {PosMax, PosMin});
part(Bin, {PosMin0, PosMax0}) ->
  BinLen = byte_size(Bin),
  case {dg_util:clip(PosMin0, 0, BinLen), dg_util:clip(PosMax0, 0, BinLen)} of
    {A, A}-><<>>;
    {PosMin, PosMax}->binary:part(Bin, {PosMin, PosMax-PosMin})
  end;
part(Bin, Len)when is_integer(Len)->
  part(Bin, {0, Len}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

from(V)when is_list(V)->
  list_to_binary(V);
from(V)when is_binary(V)->
  V;
from(V)when is_integer(V)->
  integer_to_binary(V);
from(V)when is_float(V)->
  float_to_binary(V);
from(V)when is_atom(V)->
  list_to_binary(atom_to_list(V)).

convert(V) when is_binary(V)-> {ok, V};
convert(V) when is_list(V)-> {ok, list_to_binary(V)};
convert(V) when is_integer(V)-> {ok, integer_to_binary(V)};
convert(V) when is_float(V)-> {ok, float_to_binary(V)};
convert(V)when is_atom(V)-> {ok, list_to_binary(atom_to_list(V))};
convert(_)->{error, not_supported}.