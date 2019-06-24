%%%-------------------------------------------------------------------
%%% @author pei
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 十一月 2018 14:38
%%%-------------------------------------------------------------------
-module(gz_file).
-author("pei").

%% API
-export([extension/2, full_name/2]).


extension(F, Def)->
  case filename:extension(F) of
    <<>>->Def;
    []->Def;
    O->O
  end.

full_name(Path, Name)->
  case tl(Path) of
    $/->iolist_to_binary([Path, Name]);
    _->iolist_to_binary([Path, "/", Name])
  end.