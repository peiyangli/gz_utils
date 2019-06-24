%%%-------------------------------------------------------------------
%%% @author pei
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. 十一月 2018 20:05
%%%-------------------------------------------------------------------
-module(gz_util).
-author("pei").

%% API
-export([]).

-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
for(Max, Max, F) -> [F(Max)];
for(I, Max, F) -> [F(I)|for(I+1, Max, F)].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ~s, ~b, ~f
% ~F.P.PadModC (field width, precision, padding character, control sequence modifier)
% eg. dg_util:format(<<"hello ~s"/utf8>>, ["world"]).
format(Fmt, Args)when is_binary(Fmt)->
  list_to_binary(io_lib:format(Fmt, Args));
format(Fmt, Args)->
  lists:flatten(io_lib:format(Fmt, Args)).
%% 移除首尾空格
%% string:strip("  hi  this is erlang !  ").

%% 格式化输出补零
%% io:format("~8..0B~n", [42]).
%% io:format("~10B~n", [73]).

%% 格式化右侧Padding
%% io:format("~-6B~n", [1024]).
%% io_lib:format("~-6B~n", [1024]).

%% 浮点数格式化输出
%5 io:format("~.2f~n", [1/3]).

%% 移除List重复的元素
%% lists:usort([18, 16, 17, 18, 16, 19, 14, 17, 19, 18]).


%% 文件名提取
%% string:split(<<"ab..bc..cd">>, ".", trailing).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% binary to hex
b2h(Bin) -> lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(Bin)]).
%%% hex to binary
h2b(String) -> << << (erlang:list_to_integer([Char], 16)):4/integer >> || Char <- String >>.
t(String)-> h2b(String).

%%% text only
txt(Bin) -> [X || <<X>> <= Bin,X > 32, X < 127, X =/= 45].

%%% binary to string, text only
b2s(Bin) ->    b2s1(binary_to_list(Bin),[]).
b2s1([],Str) ->    lists:reverse(Str);
b2s1([H|T],Str) ->
  case H > 32 andalso H < 127 andalso H =/= 45 of
    true -> b2s1(T,[H|Str]);
    false -> b2s1(T,[46,46|Str])
  end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% pmap, mapreduce
%% eg. pmap(fun(N)->N*2 end, [1,2,3,4]).
pmap(F,L)->pmap(F, L, self()).
pmap(F, L,P) -> [receive {Pid, Res} -> Res end || Pid <- [spawn(fun() -> P ! {self(), F(X)} end) || X <- L]].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% timer, see erlang timer
timer(Time,Fun) -> spawn(fun() -> receive after Time -> ?MODULE:Fun() end end).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 添加协议头
addLen(Bin) ->
  Len=erlang:size(Bin)+2,
  <<Len:16,Bin/binary>>.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


md5(IoList)->
  binary_to_hex(erlang:md5(IoList)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 产生GUID
random_token() ->
  Term = term_to_binary({node(), make_ref()}),
  Digest = erlang:md5(Term),
  binary_to_hex(Digest).

binary_to_hex(Bin) when is_binary(Bin) ->
  list_to_binary([oct_to_hex(N) || <<N:4>> <= Bin]).

oct_to_hex(0) -> $0;
oct_to_hex(1) -> $1;
oct_to_hex(2) -> $2;
oct_to_hex(3) -> $3;
oct_to_hex(4) -> $4;
oct_to_hex(5) -> $5;
oct_to_hex(6) -> $6;
oct_to_hex(7) -> $7;
oct_to_hex(8) -> $8;
oct_to_hex(9) -> $9;
oct_to_hex(10) -> $a;
oct_to_hex(11) -> $b;
oct_to_hex(12) -> $c;
oct_to_hex(13) -> $d;
oct_to_hex(14) -> $e;
oct_to_hex(15) -> $f.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% YA生成随机字符串

-define(SAFE_CHARS, {$a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m,
  $n, $o, $p, $q, $r, $s, $t, $u, $v, $w, $x, $y, $z,
  $A, $B, $C, $D, $E, $F, $G, $H, $I, $J, $K, $L, $M,
  $N, $O, $P, $Q, $R, $S, $T, $U, $V, $W, $X, $Y, $Z,
  $0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $_}).

rngchars(0) ->
  "";
rngchars(N) ->
  [rngchar() | rngchars(N - 1)].

rngchar() ->
  rngchar(rand:uniform(tuple_size(?SAFE_CHARS))).

rngchar(C) ->
  element(C, ?SAFE_CHARS).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 保证应用程序已经启动
ensure_started(App) ->
  case application:start(App) of
    ok ->
      ok;
    {error, {already_started, App}} ->
      ok
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clip(V, Min, Max) when Max < Min ->
  clip(V, Max, Min);
clip(V, Min, Max) ->
  if
    V < Min -> Min;
    V > Max -> Max;
    true->V
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% index nodes by ip:string -> [{node:atom, [Srvs]}]

% a_b_c@IP -> #{IP=>{a_b_c@IP, [a,b,c]}}
nodes_map()->
  case nodes() of
    []->{error, empty};
    Nodes->
      {ok, do_nodes_map(Nodes)}
  end.

do_nodes_map(Nodes)->
  lists:foldl(
    fun(K, Acc)->
      [IP, Srvs] = dg_list:split($@, atom_to_list(K)),
      Acc#{IP => maps:get(IP, Acc, []) ++ [{K, dg_list:split($_, Srvs)}]}
    end, #{}, Nodes).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 2^n = N
is2n(0)->false;
is2n(N)->
  N band (N-1) =:= 0.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%normal_range(Len, {PA0, PB0}) when PA0 < 0->
%%  PA1 = Len + PA0,
%%  if
%%    PA1 < 0 ->error;
%%    true->normal_range(Len, {PA1, PB0})
%%  end;
%%normal_range(Len, {PA0, PB0}) when PB0 < 0->
%%  PB1 = Len + PB0,
%%  if
%%    PB1 < 0 ->error;
%%    true->normal_range(Len, {PA0, PB1})
%%  end;
%%normal_range(Len, {PA0, PB0}) when PB0 > Len ->
%%  normal_range(Len, {PA0, Len});
%%normal_range(Len, {PA0, PB0}) when PA0 > Len->
%%  normal_range(Len, {Len, PB0});
%%normal_range(Len, {PA0, PB0}) when PA0>PB0->
%%  normal_range(Len, {PB0, PA0});
%%normal_range(_, V) ->
%%  {ok, V}.
normal_range(Len, {PA0, PB0}) when PA0 < 0->
  PA1 = Len + PA0 + 1,
  if
    PA1 < 0 ->error;
    true->normal_range(Len, {PA1, PB0})
  end;
normal_range(Len, {PA0, PB0}) when PB0 < 0->
  PB1 = Len + PB0 + 1,
  if
    PB1 < 0 ->error;
    true->normal_range(Len, {PA0, PB1})
  end;
normal_range(Len, {PA0, PB0}) when PB0 > Len ->
  normal_range(Len, {PA0, Len});
normal_range(Len, {PA0, PB0}) when PA0 > Len->
  normal_range(Len, {Len, PB0});
normal_range(Len, {PA0, PB0}) when PA0>PB0->
  normal_range(Len, {PB0, PA0});
normal_range(_, V) ->
  {ok, V}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
data_length(Data)when is_binary(Data)->byte_size(Data);
data_length(Data)->length(Data).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
flags_has(V, Flags)when is_integer(V) andalso is_integer(Flags)->
  (V band Flags) =/= 0;
flags_has(_, _)->false.

flags_set(V, Flags)when is_integer(V) andalso is_integer(Flags)->
  V bor Flags.

flags_remove(V, Flags)when is_integer(V) andalso is_integer(Flags)->
  Flags band (bnot V).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% require jiffy
json_decode(JsonBin)->
  try
    JsonMap = jiffy:decode(JsonBin, [return_maps]),
    {ok, JsonMap}
  catch
    E:R  ->
      {error, {E, R}}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_env(AppName, Key) ->
  get_env(AppName, Key, undefined).
get_env(AppName, Key, Default) ->
  case application:get_env(AppName, Key) of
    undefined -> Default;
    {ok, Value} -> Value
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
int_bits(N)->int_bits(N, []).

int_bits(0, Acc)-> Acc;
int_bits(N, Acc)->
  int_bits(N div 10, [N rem 10|Acc]).


is_true(false)->false;
is_true(0)->false;
is_true(_)-> true.


is_false(false)->true;
is_false(0)->true;
is_false(_)->false.