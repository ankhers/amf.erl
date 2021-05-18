-module(amf0_test).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

boolean_test() ->
    BinFalse = amf0:encode(false),
    ?assert({ok, false, <<>>} =:= amf0:decode(BinFalse)),

    BinTrue = amf0:encode(true),
    ?assert({ok, true, <<>>} =:= amf0:decode(BinTrue)),
    ok.

null_test() ->
    Bin = amf0:encode(null),
    ?assert({ok, null, <<>>} =:= amf0:decode(Bin)),
    ok.

undefined_test() ->
    Bin = amf0:encode(undefined),
    ?assert({ok, undefined, <<>>} =:= amf0:decode(Bin)),
    ok.
