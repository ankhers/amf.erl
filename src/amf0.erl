-module(amf0).

-define(NUMBER_MARKER, 16#00).
-define(BOOLEAN_MARKER, 16#01).
-define(STRING_MARKER, 16#02).
-define(OBJECT_MARKER, 16#03).
% -define(MOVIECLIP_MARKER, 16#04). Reserved, not supported
-define(NULL_MARKER, 16#05).
-define(UNDEFINED_MARKER, 16#06).
-define(REFERENCE_MARKER, 16#07).
-define(ECMA_ARRAY_MARKER, 16#08).
-define(OBJECT_END_MARKER, 16#09).
-define(STRICT_ARRAY_MARKER, 16#0A).
-define(DATE_MARKER, 16#0B).
-define(LONG_STRING_MARKER, 16#0C).
-define(UNSUPPORTED_MARKER, 16#0D).
% -define(RECORDSET_MARKER, 16#0E). Reserved, not supported
-define(XML_DOCUMENT_MARKER, 16#0F).
-define(TYPED_OBJECT_MARKER, 16#10).
-define(AVMPLUS_OBJECT_MARKER, 16#11).

-export([decode/1, encode/1, decode_key/1, encode_key/1]).

-type amf0_value() ::
    number()
    | boolean()
    | binary()
    | {amf0_object, map()}
    | null
    | undefined
    | reference
    | list(amf0_value)
    | object_end
    | array:array()
    | {amf0_date, non_neg_integer()}
    | unsupported
    | {amf0_xml_document, binary()}
    | {amf0_typed_object, binary(), map()}
    | avmplus.

-export_type([amf0_value/0]).

-spec decode(binary()) -> {ok, amf0_value(), binary()} | {error, term()}.
decode(<<Marker:8, Rest/binary>>) ->
    case Marker of
        ?NUMBER_MARKER ->
            decode_number(Rest);
        ?BOOLEAN_MARKER ->
            decode_boolean(Rest);
        ?STRING_MARKER ->
            decode_string(Rest);
        ?OBJECT_MARKER ->
            decode_object(Rest);
        ?NULL_MARKER ->
            decode_null(Rest);
        ?UNDEFINED_MARKER ->
            decode_undefined(Rest);
        ?REFERENCE_MARKER ->
            decode_reference(Rest);
        ?ECMA_ARRAY_MARKER ->
            decode_ecma_array(Rest);
        ?OBJECT_END_MARKER ->
            decode_object_end(Rest);
        ?STRICT_ARRAY_MARKER ->
            decode_strict_array(Rest);
        ?DATE_MARKER ->
            decode_date(Rest);
        ?LONG_STRING_MARKER ->
            decode_long_string(Rest);
        ?UNSUPPORTED_MARKER ->
            decode_unsupported(Rest);
        ?XML_DOCUMENT_MARKER ->
            decode_xml_document(Rest);
        ?TYPED_OBJECT_MARKER ->
            decode_typed_object(Rest);
        ?AVMPLUS_OBJECT_MARKER ->
            decode_avmplus(Rest)
    end.

-spec encode(amf0_value()) -> binary().
encode(N) when is_number(N) ->
    <<?NUMBER_MARKER:8, N/float>>;
encode(false) ->
    <<?BOOLEAN_MARKER:8, 0:8>>;
encode(true) ->
    <<?BOOLEAN_MARKER:8, 1:8>>;
encode(Bin) when is_binary(Bin) ->
    ByteSize = byte_size(Bin),
    {Marker, Size} =
        if
            ByteSize =< 16#FFFF ->
                {?STRING_MARKER, 16};
            true ->
                {?LONG_STRING_MARKER, 32}
        end,
    <<Marker:8, ByteSize:Size, Bin/binary>>;
encode({amf0_object, Map}) ->
    Object = maps:fold(
        fun(K, V, Acc) ->
            Key = encode_key(K),
            Val = encode(V),
            <<Acc/binary, Key/binary, Val/binary>>
        end,
        <<?OBJECT_MARKER:8>>,
        Map
    ),
    <<Object/binary, 0:16, ?OBJECT_END_MARKER>>;
encode(null) ->
    <<?NULL_MARKER>>;
encode(undefined) ->
    <<?UNDEFINED_MARKER>>;
encode(reference) ->
    implement_me;
encode(List) when is_list(List) ->
    {N, Bin} = lists:foldl(
        fun({K, V}, {N, Acc}) ->
            KV = encode_key_value(K, V),
            {N + 1, <<Acc/binary, KV/binary>>}
        end,
        {0, <<>>},
        List
    ),
    <<?ECMA_ARRAY_MARKER:8, N:32, Bin/binary>>;
encode({amf0_date, Millis}) ->
    <<?DATE_MARKER:8, Millis/float, 0:16>>;
encode(unsupported) ->
    <<?UNSUPPORTED_MARKER:8>>;
encode({amf0_xml_document, S}) ->
    ByteSize = byte_size(S),
    <<?XML_DOCUMENT_MARKER:8, ByteSize:32, S/binary>>;
encode({amf0_typed_object, ClassName, Data}) ->
    <<_Marker:8, Bin/binary>> = encode({amf0_object, Data}),
    ByteSize = byte_size(ClassName),
    <<?TYPED_OBJECT_MARKER:8, ByteSize:16, ClassName/binary, Bin/binary>>;
encode(avmplus) ->
    <<?AVMPLUS_OBJECT_MARKER:8>>;
encode(Array) ->
    Size = array:size(Array),
    Bin = array:foldl(
        fun(_Idx, V, Acc) ->
            Bin = encode(V),
            <<Acc/binary, Bin/binary>>
        end,
        <<>>,
        Array
    ),
    <<?STRICT_ARRAY_MARKER:8, Size:32, Bin/binary>>.

encode_key(Bin) ->
    Len = byte_size(Bin),
    <<Len:16, Bin/binary>>.

encode_key_value(Key, Value) ->
    K = encode(Key),
    V = encode(Value),
    <<K/binary, V/binary>>.

-spec decode_number(binary()) -> {ok, float(), binary()}.
decode_number(<<Float/float, Rest/binary>>) ->
    {ok, Float, Rest}.

-spec decode_boolean(binary()) -> {ok, boolean(), binary()}.
decode_boolean(<<0:8, Rest/binary>>) ->
    {ok, false, Rest};
decode_boolean(<<_:8, Rest/binary>>) ->
    {ok, true, Rest}.

-spec decode_string(binary()) -> {ok, binary(), binary()} | {error, insufficient_data}.
decode_string(<<Len:16, String:Len/binary, Rest/binary>>) ->
    {ok, String, Rest};
decode_string(_Bin) ->
    {error, insufficient_data}.

-spec decode_object(binary()) -> {ok, {amf0_object, map()}, binary()}.
decode_object(Bin) ->
    decode_object(Bin, maps:new()).

%% Object end markers are preceeded by an empty string.
%% So it would technically look like the key is <<>> and the value is object_end.
-spec decode_object(binary(), map()) -> {ok, {amf0_object, map()}, binary()}.
decode_object(Bin, Map) ->
    {ok, Key, Rest1} = decode_key(Bin),
    case decode(Rest1) of
        {ok, object_end, Rest} ->
            {ok, {amf0_object, Map}, Rest};
        {ok, Value, Rest} ->
            decode_object(Rest, maps:put(Key, Value, Map))
    end.

decode_key(<<Len:16, Rest1/binary>>) ->
    <<Key:Len/binary, Rest/binary>> = Rest1,
    {ok, Key, Rest}.

-spec decode_null(binary()) -> {ok, null, binary()}.
decode_null(Rest) ->
    {ok, null, Rest}.

-spec decode_undefined(binary()) -> {ok, undefined, binary()}.
decode_undefined(Rest) ->
    {ok, undefined, Rest}.

decode_reference(<<Number:16, Rest/binary>>) ->
    {ok, {reference, Number}, Rest}.

-spec decode_ecma_array(binary()) -> {ok, list(), binary()}.
decode_ecma_array(<<Count:32, Rest/binary>>) ->
    decode_ecma_array(Rest, Count, []).

-spec decode_ecma_array(binary(), non_neg_integer(), list()) -> {ok, list(), binary()}.
decode_ecma_array(Rest, 0, List) ->
    {ok, List, Rest};
decode_ecma_array(Bin, Count, List) ->
    {ok, Key, Rest1} = decode(Bin),
    {ok, Value, Rest} = decode(Rest1),
    NewList = [{Key, Value} | List],
    decode_ecma_array(Rest, Count - 1, NewList).

-spec decode_object_end(binary()) -> {ok, object_end, binary()}.
decode_object_end(Bin) ->
    {ok, object_end, Bin}.

-spec decode_strict_array(binary()) -> {ok, array:array(), binary()}.
decode_strict_array(<<Count:32, Rest/binary>>) ->
    decode_strict_array(Rest, 0, Count, array:new(Count)).

-spec decode_strict_array(binary(), non_neg_integer(), non_neg_integer(), array:array()) ->
    {ok, array:array(), binary()}.
decode_strict_array(Rest, Max, Max, Array) ->
    {ok, Array, Rest};
decode_strict_array(Bin, Idx, Max, Array) ->
    {ok, Value, Rest} = decode(Bin),
    decode_strict_array(Rest, Idx + 1, Max, array:set(Idx, Value, Array)).

-spec decode_date(binary()) -> {ok, {amf0_date, float()}, binary()}.
decode_date(<<Millis/float, 16#0000:16, Rest/binary>>) ->
    {ok, {amf0_date, Millis}, Rest}.

-spec decode_long_string(binary()) -> {ok, binary(), binary()}.
decode_long_string(<<Len:32, String:Len/binary, Rest/binary>>) ->
    {ok, String, Rest}.

-spec decode_unsupported(binary()) -> {ok, unsupported, binary()}.
decode_unsupported(Bin) ->
    {ok, unsupported, Bin}.

-spec decode_xml_document(binary()) -> {ok, {amf0_xml_document, binary()}, binary()}.
decode_xml_document(Bin) ->
    {ok, String, Rest} = decode_long_string(Bin),
    {ok, {amf0_xml_document, String}, Rest}.

-spec decode_typed_object(binary()) -> {ok, {amf0_typed_object, binary(), map()}, binary()}.
decode_typed_object(Bin) ->
    {ok, ClassName, Rest1} = decode_string(Bin),
    {ok, {amf0_object, Map}, Rest} = decode_object(Rest1, maps:new()),
    {ok, {amf0_typed_object, ClassName, Map}, Rest}.

decode_avmplus(Bin) ->
    {ok, avmplus, Bin}.
