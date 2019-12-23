%%%-------------------------------------------------------------------
%%% @author pei
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. 十一月 2018 20:03
%%%-------------------------------------------------------------------
-module(gz_map).
-author("pei").

%% API
-export([path/2, merge/2, from_json/1, removes/2]).

%%-compile(export_all).



%%%% get deep map value by path

path(Path, Map)->
  path(Path, Map, undefined).

path([], _, Def)->
  Def;
path([Key], Map, Def) when is_map(Map)->
  maps:get(Key, Map, Def);
path([Key|Path], Map, Def) when is_map(Map)->
  path(Path, maps:get(Key, Map), Def);
path([Key], List, Def) when is_integer(Key)->
  gz_list:nth(Key, List, Def);
path([Key|Path], List, Def) when is_integer(Key)->
  path(Path, gz_list:nth(Key, List, undefined), Def);
path(_, _, Def)->
  Def.



% deep merge of maps. when the value is both map, we do it deep merge
% replace A's with B's if the key is same
merge(A, B) when is_map(A) andalso is_map(B)->
  maps:fold(
    fun(K, V, Acc) when is_map(V)->
      case maps:find(K, Acc) of
        {ok, Value} when is_map(Value)->
          Acc#{K => merge(Value, V)};
        _->
          Acc#{K => V}
      end;
      (K, V, Acc)->
        Acc#{K => V}
    end, A, B).


from_json(JsonBin)->
  try
    JsonMap = jiffy:decode(JsonBin, [return_maps]),
    {ok, JsonMap}
  catch
    E:R  ->
      {error, {E, R}}
  end.

removes([], Map0) -> Map0;
removes([Key|Tail], Map0) -> removes(Tail, maps:remove(Key, Map0));
removes(_, Map0) -> Map0.