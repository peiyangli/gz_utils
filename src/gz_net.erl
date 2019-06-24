%%%-------------------------------------------------------------------
%%% @author pei
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. 十一月 2018 20:50
%%%-------------------------------------------------------------------
-module(gz_net).
-author("pei").

%% API
-export([]).
-compile(export_all).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
local_ip_v4() ->
  {ok, Addrs} = inet:getifaddrs(),
  hd([
    Addr || {_, Opts} <- Addrs, {addr, Addr} <- Opts,
    size(Addr) == 4, Addr =/= {127,0,0,1}
  ]).

for_addrs(Filter) ->
  case inet:getifaddrs() of
    {ok, Addrs} ->{ok, [Addr || {_, Opts} <- Addrs, {addr, Addr} <- Opts, Filter(Addr)]};
    Err->Err
  end.


%%  私有IP：
%%  A类  10.0.0.0-10.255.255.255
%%  B类  172.16.0.0-172.31.255.255
%%  C类  192.168.0.0-192.168.255.255
%%  当然，还有127这个网段是环回地址

ip_is_public_v4({10, _, _, _})-> false; % 1-9, 11-126
ip_is_public_v4({127, _, _, _})-> false; % not 127
ip_is_public_v4({172, N, _, _}) when N>15 andalso N<32 -> false; % 16-31
ip_is_public_v4({192, 168, _, _})-> false;
ip_is_public_v4({_, _, _, _})-> true;
ip_is_public_v4(_)-> false.

-define(MAX_UINT, 16#ffffffff).
ip_mask(N) when N < 32 andalso N > 0->
  ?MAX_UINT band (?MAX_UINT bsl (32 - N)).

-define(ip(A,B,C,D),
  (((A) bor (B) bor (C) bor (D)) band (bnot 16#ff)) =:= 0).

ip_to_int32(IpInt32)when is_integer(IpInt32)->IpInt32 band ?MAX_UINT;
ip_to_int32({A, B, C, D}) when ?ip(A, B, C, D)->
  (A bsl 24) + (B bsl 16) + (C bsl 8) + D.

ip_from_int32(IpInt32) when is_integer(IpInt32)->
  <<A:8,B:8,C:8,D:8>> = <<(IpInt32 band ?MAX_UINT):32>>,
  {A,B,C,D};
ip_from_int32({A, B, C, D})when ?ip(A, B, C, D)->{A, B, C, D}.

ip_range({A, B, C, D}, Msk)->
  {Min, Max} = ip_range_int32(ip_to_int32({A, B, C, D}), Msk),
  {ip_from_int32(Min), ip_from_int32(Max)}.

ip_range_int32(IpInt32, Msk)when is_integer(IpInt32) andalso Msk < 32 andalso Msk > 0->
  L = 32 - Msk,
  Min = ?MAX_UINT band IpInt32 band (?MAX_UINT bsl L),
  {Min + 1, Min + (1 bsl L)-1}.

ip_is_subnet(A0, {B0, Msk})->
  A= ip_to_int32(A0),
  B= ip_to_int32(B0),
  {Min, Max} = ip_range_int32(B, Msk),
  A > Min andalso A < Max.


ip_to_bin({A, B, C, D})when ?ip(A, B, C, D)->
  dg_util:format(<<"~b.~b.~b.~b">>, [A,B,C,D]).

