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

unsupported_test() ->
    Bin = amf0:encode(unsupported),
    ?assert({ok, unsupported, <<>>} =:= amf0:decode(Bin)),
    ok.

avm_plus_test() ->
    Bin = amf0:encode(avmplus),
    ?assert({ok, avmplus, <<>>} =:= amf0:decode(Bin)),
    ok.

map_test() ->
    Bin =
        <<3, 0, 3, 97, 112, 112, 2, 0, 0, 0, 4, 116, 121, 112, 101, 2, 0, 10, 110, 111, 110, 112,
            114, 105, 118, 97, 116, 101, 0, 8, 102, 108, 97, 115, 104, 86, 101, 114, 2, 0, 31, 70,
            77, 76, 69, 47, 51, 46, 48, 32, 40, 99, 111, 109, 112, 97, 116, 105, 98, 108, 101, 59,
            32, 70, 77, 83, 99, 47, 49, 46, 48, 41, 0, 6, 115, 119, 102, 85, 114, 108, 2, 0, 16,
            114, 116, 109, 112, 58, 47, 47, 108, 111, 99, 97, 108, 104, 111, 115, 116, 0, 5, 116,
            99, 85, 114, 108, 2, 0, 16, 114, 116, 109, 112, 58, 47, 47, 108, 111, 99, 97, 108, 104,
            111, 115, 116, 0, 0, 9>>,

    {ok, Map, <<>>} = amf0:decode(Bin),
    io:format("Map: ~p~n", [Map]),
    ok.
