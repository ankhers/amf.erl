-module(prop_amf0).

-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_numbers() ->
    ?FORALL(
        Float,
        amf0_number(),
        begin
            Bin = amf0:encode(Float),
            {ok, Float, <<>>} == amf0:decode(Bin)
        end
    ).

prop_strings() ->
    ?FORALL(
        Str,
        amf0_string(),
        begin
            Bin = amf0:encode(Str),
            {ok, Str, <<>>} == amf0:decode(Bin)
        end
    ).

prop_object() ->
    ?FORALL(
        {amf0_object, Map1} = Obj,
        amf0_object(),
        begin
            Bin = amf0:encode(Obj),
            {ok, {amf0_object, Map2}, <<>>} = amf0:decode(Bin),
            deep_compare(Map1, Map2)
        end
    ).

prop_ecma_array() ->
    ?FORALL(
        List,
        amf0_ecma_array(),
        begin
            Bin = amf0:encode(List),
            {ok, NewList, <<>>} = amf0:decode(Bin),
            List == lists:reverse(NewList)
        end
    ).

prop_strict_array() ->
    ?FORALL(
        {array, Size, _, Val, _} = Array,
        amf0_strict_array(),
        begin
            Bin = amf0:encode(Array),
            {ok, {array, Size, _, Val, _}, <<>>} = amf0:decode(Bin),
            true
        end
    ).

prop_date() ->
    ?FORALL(
        {amf0_date, Millis} = Date,
        amf0_date(),
        begin
            Bin = amf0:encode(Date),
            {ok, {amf0_date, Millis}, <<>>} == amf0:decode(Bin)
        end
    ).

prop_long_string() ->
    ?FORALL(
        String,
        amf0_long_string(),
        begin
            Bin = amf0:encode(String),
            {ok, String, <<>>} = amf0:decode(Bin),
            true
        end
    ).

prop_xml_document() ->
    ?FORALL(
        Doc,
        amf0_xml_document(),
        begin
            Bin = amf0:encode(Doc),
            {ok, Doc, <<>>} = amf0:decode(Bin),
            true
        end
    ).

prop_typed_object() ->
    ?FORALL(
        {amf0_typed_object, ClassName, Map1} = Obj,
        amf0_typed_object(),
        begin
            Bin = amf0:encode(Obj),
            {ok, {amf0_typed_object, ClassName, Map2}, <<>>} = amf0:decode(Bin),
            deep_compare(Map1, Map2)
        end
    ).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
deep_compare(Map1, Map2) ->
    List = maps:to_list(Map1),
    compare_kv(List, Map2).

compare_kv([], #{}) ->
    true;
compare_kv([{K, V} | Tail], Map) ->
    Ret =
        case maps:get(K, Map, unknown_key) of
            unknown_key ->
                false;
            V2 ->
                compare_values(V, V2)
        end,
    if
        Ret ->
            compare_kv(Tail, maps:remove(K, Map));
        true ->
            false
    end.

compare_values({amf0_typed_object, ClassName, Map1}, {amf0_typed_object, ClassName, Map2}) ->
    deep_compare(Map1, Map2);
compare_values({amf0_object, Map1}, {amf0_object, Map2}) ->
    deep_compare(Map1, Map2);
compare_values(Map1, Map2) when is_map(Map1) andalso is_map(Map2) ->
    deep_compare(Map1, Map2);
compare_values({array, Size, _, Val, _}, {array, Size, _, Val, _}) ->
    true;
compare_values(List1, List2) when is_list(List1) andalso is_list(List2) ->
    List1 == lists:reverse(List2);
compare_values(V1, V2) ->
    V1 == V2.

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
amf0_number() -> number().

amf0_string() -> ?LET(N, range(0, 16#FFFF), binary(N)).

amf0_object() -> ?LET(Map, map(binary(), amf0_any()), {amf0_object, Map}).

amf0_ecma_array() -> list({binary(), binary()}).

amf0_strict_array() -> ?LET(List, list(binary()), array:from_list(List)).

amf0_date() -> ?LET(N, non_neg_integer(), {amf0_date, N}).

amf0_long_string() -> ?LET(N, non_neg_integer(), binary(N + 16#FFFF + 1)).

amf0_xml_document() ->
    ?LET(S, oneof([amf0_string(), amf0_long_string()]), {amf0_xml_document, S}).

amf0_typed_object() ->
    ?LET(
        {ClassName, {_, Map}},
        {amf0_string(), amf0_object()},
        {amf0_typed_object, ClassName, Map}
    ).

amf0_any() ->
    frequency([
        {10, amf0_number()},
        {10, amf0_string()},
        {1, ?LAZY(amf0_object())},
        {10, amf0_ecma_array()},
        {10, amf0_strict_array()},
        {10, amf0_long_string()},
        {10, amf0_xml_document()},
        {1, ?LAZY(amf0_typed_object())},
        {10, null},
        {10, undefined},
        {10, boolean()}
    ]).
