%%%-------------------------------------------------------------------
%%% @author pei
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 十二月 2018 19:44
%%%-------------------------------------------------------------------
-module(gz_uri).
-author("pei").

%% API
-export([encode/1, decode/1]).
-export([uni_encode/2, uni_encode/3, uni_encode/1]).

%%%======================================================================================================================================
uni_encode(T0)->
  T = unicode:characters_to_binary(T0, latin1, utf8),
  encode(T).
uni_encode(T0, UniO)->
  T = unicode:characters_to_binary(T0, latin1, UniO),
  encode(T).
uni_encode(T0, UniI, UniO)->
  T = unicode:characters_to_binary(T0, UniI, UniO),
  encode(T).

%%%======================================================================================================================================

encode(URI) when is_list(URI) ->
  lists:append([uri_encode(Char) || Char <- URI]);
encode(URI) when is_binary(URI) ->
  << <<(uri_encode_binary(Char))/binary>> || <<Char>> <= URI >>.

decode(String) when is_list(String) ->
  do_decode(String);
decode(String) when is_binary(String) ->
  do_decode_binary(String).

do_decode([$%,Hex1,Hex2|Rest]) ->
  [hex2dec(Hex1)*16+hex2dec(Hex2)|do_decode(Rest)];
do_decode([$+|Rest]) ->
  [$ |do_decode(Rest)];
do_decode([First|Rest]) ->
  [First|do_decode(Rest)];
do_decode([]) ->
  [].

do_decode_binary(<<$%, Hex:2/binary, Rest/bits>>) ->
  <<(binary_to_integer(Hex, 16)), (do_decode_binary(Rest))/binary>>;
do_decode_binary(<<$+, Rest/bits>>) ->
  <<$ , (do_decode_binary(Rest))/binary>>;
do_decode_binary(<<First:1/binary, Rest/bits>>) ->
  <<First/binary, (do_decode_binary(Rest))/binary>>;
do_decode_binary(<<>>) ->
  <<>>.

uri_encode(X)when (X>=$0) andalso (X=<$9) -> [X];
uri_encode(X)when (X>=$A) andalso (X=<$Z) -> [X];
uri_encode(X)when (X>=$a) andalso (X=<$z) -> [X];
uri_encode($-)->[$-];
uri_encode($_)->[$_];
uri_encode($.)->[$.];
uri_encode($*)->[$*];
uri_encode($ ) ->[$+];
uri_encode(X) ->[ $% | integer_to_list(X, 16)].

uri_encode_binary(X)when (X>=$0) andalso (X=<$9) -> <<X>>;
uri_encode_binary(X)when (X>=$A) andalso (X=<$Z) -> <<X>>;
uri_encode_binary(X)when (X>=$a) andalso (X=<$z) -> <<X>>;
uri_encode_binary($-)-> <<$->>;
uri_encode_binary($_)-> <<$_>>;
uri_encode_binary($.)-> <<$.>>;
uri_encode_binary($*)-> <<$*>>;
uri_encode_binary($ )-> <<$+>>;
uri_encode_binary(X) -><< $%, (integer_to_binary(X, 16))/binary >>.


hex2dec(X) when (X>=$0) andalso (X=<$9) -> X-$0;
hex2dec(X) when (X>=$A) andalso (X=<$F) -> X-$A+10;
hex2dec(X) when (X>=$a) andalso (X=<$f) -> X-$a+10.
%%%======================================================================================================================================