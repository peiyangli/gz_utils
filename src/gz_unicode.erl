%%%-------------------------------------------------------------------
%%% @author pei
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 十一月 2018 16:32
%%%-------------------------------------------------------------------
-module(gz_unicode).
-author("pei").

%% API
-export([replace/3]).

replace(Data0, Value, Range) when is_list(Data0)->
  Data1 = unicode:characters_to_list(Data0),
  Data2 = dg_list:replace(Data1, Value, Range),
  lists:flatten(Data2);
replace(Data0, Value, Range)->
  Data1 = unicode:characters_to_list(Data0),
  Data2 = dg_list:replace(Data1, Value, Range),
  unicode:characters_to_binary(Data2).