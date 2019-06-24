%%%-------------------------------------------------------------------
%%% @author pei
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. 十一月 2018 20:03
%%%-------------------------------------------------------------------
-module(gz_list).
-author("pei").

%% API
-export([cross/2, delete/2, unique/1, difference/2, unique_difference/2, split/2, chain/2, tripart/2, part/2, shift/2, random_shift/1, nth/3, unique/2, replace/3, classify/2, same/1]).
%%-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% set cross
cross(L1,L2) ->
  [X||X<-L1,Y<-L2,X=:=Y].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% delete all item in
delete(Item, [Item|Rest])->
  delete(Item, Rest);
delete(Item, [H|Rest])->
  [H|delete(Item, Rest)];
delete(_, []) -> [].


delete_item(Mems, undefined)->Mems;
delete_item(Mems, Items) when is_list(Items)->
  lists:foldl(fun(Item, Acc)->lists:delete(Item, Acc) end, Mems, Items);
delete_item(Mems, Item)->lists:delete(Item, Mems).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% make list unique say a set
unique(L)->
  do_unique(L, []).

do_unique([], Uni)->
  Uni;
do_unique([Item|Rest], Uni)->
  case lists:member(Item, Uni) of
    true->
      do_unique(Rest, Uni);
    _->
      do_unique(Rest, [Item|Uni])
  end.

% Pre = fun(A,B)->A=:=B end
unique(Pre, L)->
  do_unique(Pre, L, []).

do_unique(_, [], Uni)->
  lists:reverse(Uni);
do_unique(Pre, [Item|Rest], Uni)->
  case lists:search(fun(I)->Pre(Item, I) end, Uni) of
    false->
      do_unique(Pre, Rest, [Item|Uni]);
    _->
      do_unique(Pre, Rest, Uni)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
difference(L1, L2) when is_list(L1) andalso is_list(L2)-> L1 -- L2;
difference(L1, L2) when is_list(L1)-> L1 -- [L2];
difference(_, _)-> [].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
unique_difference(L, D)->
  lists:foldl(
    fun(Item, Acc)->
      case lists:member(Item, D) of
        true->Acc;
        _->
          case lists:member(Item, Acc) of
            true->Acc;
            _->[Item|Acc]
          end
      end
    end, [], L).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% split list by X
% eg. split(" _", "hello this is a_test").
split(Pre, List) when is_function(Pre)->
  split(Pre, List, [], []);
split(Pre, List) when is_list(Pre)->
  split(fun(X)-> lists:member(X, Pre) end, List, [], []);
split(Pre, List)->
  split(fun(X)-> X=:=Pre end, List, [], []).

split(_Pre, [], AccList, AccItem)->
  case AccItem of
    []->
      AccList;
    _->
      [lists:reverse(AccItem)|AccList]
  end;
split(Pre, [A|Tail], AccList, AccItem)->
  case Pre(A) of
    true ->
      case AccItem of
        []->
          split(Pre, Tail, AccList, []);
        _->
          split(Pre, Tail, [lists:reverse(AccItem)|AccList], [])
      end;
    _ ->
      split(Pre, Tail, AccList, [A|AccItem])
  end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% chain call functions, return {ok, ToNext}
chain([Hd|Tail], Acc0) when is_function(Hd, 1)->
  case Hd(Acc0) of
    {ok, Acc}->
      chain(Tail, Acc);
    Err->Err
  end;
chain([], Acc0) ->
  {ok, Acc0}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%lists:tripart() to 3
%{true, false, Other}
tripart(Pred, List)->
  tripart(Pred, List, [], [], []).

tripart(Pred, [H | T], As, Bs, Cs) ->
  case Pred(H) of
    true -> tripart(Pred, T, [H | As], Bs, Cs);
    false -> tripart(Pred, T, As, [H|Bs], Cs);
    _ -> tripart(Pred, T, As, Bs, [H|Cs])
  end;
tripart(_, [], As, Bs, Cs) ->
  {lists:reverse(As), lists:reverse(Bs), lists:reverse(Cs)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% part of list {posA, posB}
part([], _)->
  [];
part(_, {P, P})->
  [];
part(List, {PosMin, PosMax}) when PosMax < PosMin->
  part(List, {PosMax, PosMin});
part(List, {PosMin0, PosMax0}) ->
  DataLen = length(List),
  case {dg_util:clip(PosMin0, 0, DataLen), dg_util:clip(PosMax0, 1, DataLen)} of
    {A, A}->[];
    {PosMin, PosMax}->lists:sublist(List, PosMin+1, PosMax-PosMin)
  end;
part(List, Len)when is_integer(Len)->
  part(List, {0, Len}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% shift a list,

shift(0, L)->
  L;
shift(_, [])->
  [];
shift(_, L = [_])->
  L;
shift(N0, L)->
  Len = length(L),
  N = N0 rem Len,
  do_list_shift(N, L, Len).

do_list_shift(0, L, _)->L;
do_list_shift(N0, L, _) when N0 > 0->
  {L1, L2} = lists:split(N0, L),
  L2 ++ L1;
do_list_shift(N0, L, Len) when N0 < 0->
  {L1, L2} = lists:split(Len + N0, L),
  L2 ++ L1.

random_shift([])->[];
random_shift(L=[_])->L;
random_shift(L)->
  N = rand:uniform(length(L)),
  shift(N, L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% nth with default
nth(1, [H|_], _) -> H;
nth(N, [_|T], Def) when N > 1 ->
  nth(N - 1, T, Def);
nth(N, Tuple, Def) when is_tuple(Tuple)->
  try
    erlang:element(N, Tuple)
  catch
    _:_  ->Def
  end;
nth(_, _, Def)->Def.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

replace([], _, _) ->
  [];
replace(Data, Value, Range)when is_tuple(Range) ->
  DL = dg_util:data_length(Data),
  case dg_util:normal_range(DL, Range) of
    {ok, {PosA, PosB}}->
      do_list_replace_pos(Data, DL, Value, PosA, PosB - PosA);
    _->Data
  end;
replace(Data, Value, Pos0) when Pos0 < 0 ->
  DL = dg_util:data_length(Data),
  Pos = DL + Pos0 + 1,
  if
    Pos < 0 -> Data;
    true-> do_list_replace_pos(Data, DL, Value, Pos, 1)
  end;
replace(Data, Value, Pos0) ->
  DL = dg_util:data_length(Data),
  case DL - Pos0 < 0 of
    true->Data;
    _->do_list_replace_pos(Data, DL, Value, Pos0, 1)
  end.

do_list_replace_pos(Data, _, _, _, Len) when Len < 1 ->
  Data;
do_list_replace_pos(Data, DL, _, Pos0, _) when DL =< Pos0->
  Data;
do_list_replace_pos(Data, _, Value, Pos0, Len)->
  {A, B} = lists:split(Pos0, Data),
  {_, D} = lists:split(Len, B),
  %lists:flatten()
  [A, Value, D].



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Pre(Item) -> Type
% return #{Type=>[Item]}
% e.g. classify(fun(V)-> V rem 3 end, lists:seq(1, 20)).
classify(Pre, List)->
  lists:foldl(
    fun(Item, Acc)->
      Type = Pre(Item),
      Items = maps:get(Type, Acc, []),
      Acc#{ Type => [Item|Items]}
    end, #{}, List).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
same([])->true;
same([A|Left])->
  same(A, Left).

same(_, [])->true;
same(A, [A|Left])->
  same(A, Left);
same(_, _)->false.