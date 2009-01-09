%% @doc A protcol buffers encoding and decoding module.
-module(protobuffs).
-export([encode/3, decode/2, decode_many/1]).

-define(TYPE_VARINT, 0).
-define(TYPE_64BIT, 1).
-define(TYPE_STRING, 2).
-define(TYPE_START_GROUP, 3).
-define(TYPE_END_GROUP, 4).
-define(TYPE_32BIT, 5).

%% @spec encode(FieldID, Value, Type) -> Result
%%       FieldID = integer()
%%       Value = any()
%%       Type = bool | enum | int32 | uint32 | int64 | unit64 | sint32 | sint64 | fixed32 | sfixed32 | fixed64 | sfixed64 | string | bytes | float | double 
%%       Result = list()
%% @doc Encode an Erlang data structure into a Protocol Buffers value.
encode(FieldID, false, bool) ->
    encode(FieldID, 0, bool);
encode(FieldID, true, bool) ->
    encode(FieldID, 1, bool);
encode(FieldID, Integer, enum) ->
    encode(FieldID, Integer, uint32);
encode(FieldID, Integer, IntType) when IntType =:= int32,  Integer >= -16#80000000, Integer =< 16#7fffffff ->
    encode_varint_field(FieldID, Integer);
encode(FieldID, Integer, IntType) when IntType =:= uint32, Integer band 16#ffffffff =:= Integer ->
    encode_varint_field(FieldID, Integer);
encode(FieldID, Integer, IntType) when IntType =:= int64,  Integer >= -16#8000000000000000, Integer =< 16#7fffffffffffffff ->
    encode_varint_field(FieldID, Integer);
encode(FieldID, Integer, IntType) when IntType =:= uint64, Integer band 16#ffffffffffffffff =:= Integer ->
    encode_varint_field(FieldID, Integer);
encode(FieldID, Integer, IntType) when IntType =:= bool, Integer band 1 =:= 1 ->
    encode_varint_field(FieldID, Integer);
encode(FieldID, Integer, IntType) when IntType =:= sint32, Integer >= -16#80000000, Integer < 0 ->
    encode_varint_field(FieldID, bnot (Integer bsl 1));
encode(FieldID, Integer, IntType) when IntType =:= sint64, Integer >= -16#8000000000000000, Integer < 0 ->
    encode_varint_field(FieldID, bnot (Integer bsl 1));
encode(FieldID, Integer, IntType) when IntType =:= sint64, Integer >= 0, Integer =< 16#7fffffff ->
    encode_varint_field(FieldID, Integer bsl 1);
encode(FieldID, Integer, IntType) when IntType =:= sint64, Integer >= 0, Integer =< 16#7fffffffffffffff ->
    encode_varint_field(FieldID, Integer bsl 1);
encode(FieldID, Integer, fixed32) when Integer band 16#ffffffff =:= Integer ->
    [encode_field_tag(FieldID, ?TYPE_32BIT), <<Integer:32/little-integer>>];
encode(FieldID, Integer, sfixed32) when Integer >= -16#80000000, Integer =< 16#7fffffff ->
    [encode_field_tag(FieldID, ?TYPE_32BIT), <<Integer:32/little-integer>>];
encode(FieldID, Integer, fixed64) when Integer band 16#ffffffffffffffff =:= Integer ->
    [encode_field_tag(FieldID, ?TYPE_64BIT), <<Integer:64/little-integer>>];
encode(FieldID, Integer, sfixed64) when Integer >= -16#8000000000000000, Integer =< 16#7fffffffffffffff ->
    [encode_field_tag(FieldID, ?TYPE_64BIT), <<Integer:64/little-integer>>];
encode(FieldID, String, string) when is_list(String) ->
    encode(FieldID, list_to_binary(String), string);
encode(FieldID, String, string) when is_binary(String) ->
    encode(FieldID, String, bytes);
encode(FieldID, String, bytes) when is_list(String) ->
    encode(FieldID, list_to_binary(String), bytes);
encode(FieldID, Bytes, bytes) when is_binary(Bytes) ->
    [encode_field_tag(FieldID, ?TYPE_STRING), encode_varint(size(Bytes)), Bytes];
encode(FieldID, Float, float) when is_float(Float) ->
    [encode_field_tag(FieldID, ?TYPE_32BIT), <<Float:32/little-float>>];
encode(FieldID, Float, double) when is_float(Float) ->
    [encode_field_tag(FieldID, ?TYPE_64BIT), <<Float:64/little-float>>].
%% encode(FieldID, Value, _) when is_binary(Value) ->
%%     encode(FieldID, Value, bytes);
%% encode(FieldID, Value, _) when is_atom(Value) ->
%%     encode(FieldID, erlang:atom_to_list(Value), string).

%% @spec decode(Bytes, ExpectedType) -> Result
%%       Bytes = binary()
%%       ExpectedType = bool | enum | int32 | uint32 | int64 | unit64 | sint32 | sint64 | fixed32 | sfixed32 | fixed64 | sfixed64 | string | bytes | float | double 
%%       Result = {{integer(), any()}, binary()}
decode(Bytes, ExpectedType) ->
    {Tag, Rest1} = decode_varint(Bytes),
    FieldID = Tag bsr 3,
    WireType = Tag band 7,
    {Value, Rest2} = decode_value(Rest1, WireType, ExpectedType),
    {{FieldID, Value}, Rest2}.

%% @spec decode_many(Bytes) -> Results
%%       Bytes = binary()
%%       Results = [Result]
%%       Result = {integer(), any()}
decode_many(Bytes) ->
    decode_many(Bytes, []).

%% @hidden
decode_value(Bytes, ?TYPE_VARINT, ExpectedType) ->
    {Value, Rest} = decode_varint(Bytes),
    {typecast(Value, ExpectedType), Rest};
decode_value(<<Value:64/little-unsigned-integer, Rest/binary>>, ?TYPE_64BIT, fixed64) ->
    {Value, Rest};
decode_value(<<Value:32/little-unsigned-integer, _:32, Rest/binary>>, ?TYPE_64BIT, fixed32) ->
    {Value, Rest};
decode_value(<<Value:64/little-signed-integer, Rest/binary>>, ?TYPE_64BIT, sfixed64) ->
    {Value, Rest};
decode_value(<<Value:32/little-signed-integer, _:32, Rest/binary>>, ?TYPE_64BIT, sfixed32) ->
    {Value, Rest};
decode_value(<<Value:64/little-float, Rest/binary>>, ?TYPE_64BIT, Type) when Type =:= double; Type =:= float ->
    {Value, Rest};
decode_value(Bytes, ?TYPE_STRING, ExpectedType) when ExpectedType =:= string; ExpectedType =:= bytes ->
    {Length, Rest} = decode_varint(Bytes),
    split_binary(Rest, Length);
decode_value(<<Value:32/little-unsigned-integer, Rest/binary>>, ?TYPE_32BIT, Type) when Type =:= fixed32; Type =:= fixed64 ->
    {Value, Rest};
decode_value(<<Value:32/little-signed-integer, Rest/binary>>, ?TYPE_32BIT, Type) when Type =:= sfixed32; Type =:= sfixed64 ->
    {Value, Rest};
decode_value(<<Value:32/little-float, Rest/binary>>, ?TYPE_32BIT, Type) when Type =:= double; Type =:= float ->
    {Value, Rest}.

%% @hidden
typecast(Value, SignedType) when SignedType =:= int32; SignedType =:= int64 ->
    if
        Value band 16#8000000000000000 =/= 0 -> Value - 16#10000000000000000;
        true -> Value
    end;
typecast(Value, SignedType) when SignedType =:= sint32; SignedType =:= sint64 ->
    (Value bsr 1) bxor (-(Value band 1));
typecast(Value, _) ->
    Value.

%% @hidden
encode_field_tag(FieldID, FieldType) when FieldID band 16#3fffffff =:= FieldID ->
    encode_varint((FieldID bsl 3) bor FieldType).

%% @hidden
encode_varint_field(FieldID, Integer) ->
    [encode_field_tag(FieldID, ?TYPE_VARINT), encode_varint(Integer)].

%% @hidden
encode_varint(I) when I band 16#7f =:= I ->
    I;
encode_varint(I) when I band 16#3fff =:= I ->
    <<(16#80 bor (I bsr 7)), (I band 16#7f)>>;
encode_varint(I) when I band 16#1fffff =:= I ->
    <<(16#80 bor (I bsr 14)),
      (16#80 bor (I bsr 7) band 16#ff), (I band 16#7f)>>;
encode_varint(I) when I band 16#fffffff =:= I ->
    <<(16#80 bor (I bsr 21)), (16#80 bor (I bsr 14) band 16#ff),
      (16#80 bor (I bsr 7) band 16#ff), (I band 16#7f)>>;
encode_varint(I) when I band 16#7ffffffff =:= I ->
    <<(16#80 bor (I bsr 28)),
      (16#80 bor (I bsr 21) band 16#ff), (16#80 bor (I bsr 14) band 16#ff),
      (16#80 bor (I bsr 7) band 16#ff), (I band 16#7f)>>;
encode_varint(I) when I band 16#3ffffffffff =:= I ->
    <<(16#80 bor (I bsr 35)), (16#80 bor (I bsr 28) band 16#ff),
      (16#80 bor (I bsr 21) band 16#ff), (16#80 bor (I bsr 14) band 16#ff),
      (16#80 bor (I bsr 7) band 16#ff), (I band 16#7f)>>;
encode_varint(I) when I band 16#1ffffffffffff =:= I ->
    <<(16#80 bor (I bsr 42)),
      (16#80 bor (I bsr 35) band 16#ff), (16#80 bor (I bsr 28) band 16#ff),
      (16#80 bor (I bsr 21) band 16#ff), (16#80 bor (I bsr 14) band 16#ff),
      (16#80 bor (I bsr 7) band 16#ff), (I band 16#7f)>>;
encode_varint(I) when I band 16#ffffffffffffff =:= I ->
    <<(16#80 bor (I bsr 49) band 16#ff), (16#80 bor (I bsr 42) band 16#ff),
      (16#80 bor (I bsr 35) band 16#ff), (16#80 bor (I bsr 28) band 16#ff),
      (16#80 bor (I bsr 21) band 16#ff), (16#80 bor (I bsr 14) band 16#ff),
      (16#80 bor (I bsr 7) band 16#ff), (I band 16#7f)>>;
encode_varint(I) when I band 16#7fffffffffffffff =:= I ->
    <<(16#80 bor (I bsr 56)),
      (16#80 bor (I bsr 49) band 16#ff), (16#80 bor (I bsr 42) band 16#ff),
      (16#80 bor (I bsr 35) band 16#ff), (16#80 bor (I bsr 28) band 16#ff),
      (16#80 bor (I bsr 21) band 16#ff), (16#80 bor (I bsr 14) band 16#ff),
      (16#80 bor (I bsr 7) band 16#ff), (I band 16#7f)>>;
encode_varint(I) when I band 16#ffffffffffffffff =:= I ->
    <<(16#80 bor (I bsr 63) band 16#81), (16#80 bor (I bsr 56) band 16#ff),
      (16#80 bor (I bsr 49) band 16#ff), (16#80 bor (I bsr 42) band 16#ff),
      (16#80 bor (I bsr 35) band 16#ff), (16#80 bor (I bsr 28) band 16#ff),
      (16#80 bor (I bsr 21) band 16#ff), (16#80 bor (I bsr 14) band 16#ff),
      (16#80 bor (I bsr 7) band 16#ff), (I band 16#7f)>>.

%% @hidden
decode_varint(Bytes) ->
    decode_varint(Bytes, 0).

%% @hidden
decode_varint(<<0:1, I:7, Rest/binary>>, Accum) when Accum =< 16#3ffffffffffffff ->
    {Accum bsl 7 bor I, Rest};
decode_varint(<<1:1, I:7, Rest/binary>>, Accum) ->
    decode_varint(Rest, Accum bsl 7 bor I).

%% @hidden
decode_many(<<>>, Acc) -> lists:keysort(1, Acc);
decode_many(Bytes, Acc) ->
    {A, B} = decode(Bytes, bytes),
    decode_many(B, [A | Acc]).
