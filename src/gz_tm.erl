%%%-------------------------------------------------------------------
%%% @author pei
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. 十一月 2018 20:04
%%%-------------------------------------------------------------------
-module(gz_tm).
-author("pei").

%% API
-export([second/0, millisecond/0, microsecond/0, now/0]).
-export([timestamp_to_datetime/1, format/1, timestamp_to_localtime/1, timestamp_to_datetime/2, hms_to_s/1, timestamp_to_date/2]).
-export([datetime_add_second/2, date_next/1, date_next/2, datetime_to_timestamp/2, date_to_timestamp/2]).

-export([mid/1, mid/0, tm_of_mid_s/1, tm_of_mid_ms/1]).



test()->
  calendar:now_to_datetime(erlang:timestamp()).


now()->erlang:system_time(millisecond).

second()->
  erlang:system_time(second).

millisecond()->
  erlang:system_time(millisecond).

microsecond()->
  erlang:system_time(microsecond).


% Sid(0-255),
% say the N is 0-16,777,215 for one server
% and if second is same
mid(Sid)->
  S = erlang:system_time(second),
%%  S = erlang:monotonic_time(second),
  N = erlang:unique_integer([positive]),
  <<A:64>> = <<S:32, N:22, Sid:8, 0:2>>,
  {S, A}.

% it is not likely to get the same value
% 1/8388607 per ms
mid()->
  MS = erlang:system_time(millisecond),
  N = erlang:unique_integer([positive]),
  <<A:64>> = <<MS:41, N:21, 0:2>>,
  {MS, A}.

tm_of_mid_s(Mid)->
  <<S:32, _:32>> = <<Mid:64>>,
  S.

tm_of_mid_ms(Mid)->
  <<MS:41, _:23>> = <<Mid:64>>,
  MS.


% 62167219200 = calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}})).
-define(gregorian_seconds_1970_1_1, 62167219200).

timestamp_to_datetime(Timestamp) ->
  % + 62167219200
  calendar:gregorian_seconds_to_datetime(Timestamp + ?gregorian_seconds_1970_1_1).
%%    calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}})).

timestamp_to_localtime(Timestamp)->
  calendar:universal_time_to_local_time(timestamp_to_datetime(Timestamp)).

% N = [-11, +12]
timestamp_to_datetime(Timestamp, N)->
  timestamp_to_datetime(Timestamp + N*3600).

timestamp_to_date(Timestamp, N)->
  {YMD, _HMS} = timestamp_to_datetime(Timestamp, N),
  YMD.



hms_to_s({H,M,S})->
  S+M*60+H*3600.

format({{Y,Month,D}, {H,M,S}})->
  dg_util:format("~4w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w", [Y,Month,D,H,M,S]);
format(TmS)->
  format(timestamp_to_localtime(TmS)).


datetime_add_second(DateTime, Ss)->
  calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(DateTime) + Ss).

date_next(YMD)->
  {D, _} = datetime_add_second({YMD, {0,0,0}}, 86400),
  D.

date_next(YMD, N)->
  {D, _} = datetime_add_second({YMD, {0,0,0}}, N*86400),
  D.

datetime_to_timestamp(DateTime, N)->
  calendar:datetime_to_gregorian_seconds(DateTime) - ?gregorian_seconds_1970_1_1 - N*3600.

date_to_timestamp(YMD, N)->
  datetime_to_timestamp({YMD, {0,0,0}},N).