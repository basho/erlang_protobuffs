%% Copyright (c) 2009 
%% Nick Gerakines <nick@gerakines.net>
%% Jacob Vorreuter <jacob.vorreuter@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
%%
%% @doc A protcol buffers encoding and decoding module.
-module(protobuffs).
-export([encode/3, read_field_num_and_wire_type/1, decode/2, decode_value/3]).
%-compile(export_all).
%-compile({inline_size,100}).

-define(TYPE_VARINT, 0).
-define(TYPE_64BIT, 1).
-define(TYPE_STRING, 2).
-define(TYPE_START_GROUP, 3).
-define(TYPE_END_GROUP, 4).
-define(TYPE_32BIT, 5).


%% @spec encode(FieldID, Value, Type) -> Result
%%       FieldID = integer()
%%       Value = any()
%%       Type = bool | enum | int32 | uint32 | int64 | uint64 | sint32 | sint64 | fixed32 | sfixed32 | fixed64 | sfixed64 | string | bytes | float | double 
%%       Result = list()
%% @doc Encode an Erlang data structure into a Protocol Buffers value.
encode(FieldID, Value, Type) ->
%%    iolist_to_binary(encode_internal(FieldID, Value, Type)).
    encode_internal(Type, FieldID, Value).

-define(encode_field_tag(FieldID, FieldType), 
	case FieldID band 16#3fffffff of
		FieldID ->
			encode_varint((FieldID bsl 3) bor FieldType); 
		_ ->
			exit({error, {encode_field_tag, FieldID, FieldType}})
	end).
-define(encode_internal_bytes(FieldID, Bytes), [?encode_field_tag(FieldID, ?TYPE_STRING), encode_varint(size(Bytes)), Bytes]).
-define(encode_varint_field(FieldID, Integer), [?encode_field_tag(FieldID, ?TYPE_VARINT), encode_varint(Integer)]).
-define(encode_internal_float(FieldID, Float, Type, Bit),
	Float1 = case is_float(Float) of
		true -> Float;
		_ ->
			case is_integer(Float) of
				true -> Float + 0.0;
				_ -> exit({error, {encode_internal, float_or_double, FieldID, Float}})
			end
	end,
	[?encode_field_tag(FieldID, Type), <<Float1:Bit/little-float>>]).

%% @hidden
encode_internal(bytes, FieldID, <<Bytes/binary>>) ->
    ?encode_internal_bytes(FieldID, Bytes);
encode_internal(bytes, FieldID, []) ->
    ?encode_internal_bytes(FieldID, <<>>);
encode_internal(bytes, FieldID, String=[_|_]) ->
    ?encode_internal_bytes(FieldID, list_to_binary(String));
encode_internal(string, FieldID, []) ->
	?encode_internal_bytes(FieldID, <<>>);
encode_internal(string, FieldID, String = [_|_]) ->
    ?encode_internal_bytes(FieldID, list_to_binary(String));
encode_internal(string, FieldID, <<Binary/binary>>) ->
    ?encode_internal_bytes(FieldID, Binary);
encode_internal(uint32, FieldID, Integer) ->
	case Integer band 16#ffffffff of
		Integer ->
			?encode_varint_field(FieldID, Integer);
		_ ->
			exit({error, {encode_internal, uint32, FieldID, Integer}})
	end;
encode_internal(bool, FieldID, false) ->
    encode_internal(int32, FieldID, 0);
encode_internal(bool, FieldID, true) ->
    encode_internal(int32, FieldID, 1);
encode_internal(enum, FieldID, Integer) ->
    encode_internal(uint32, FieldID, Integer);
encode_internal(int32, FieldID, Integer) ->
	if Integer >= 0 andalso Integer =< 16#7fffffff ->
			?encode_varint_field(FieldID, Integer);
		Integer >= -16#80000000 andalso Integer < 0 ->
			encode_internal(int64, FieldID, Integer);
		true ->
			exit({error, {encode_internal, int32, FieldID, Integer}})
	end;
encode_internal(int64, FieldID, Integer) ->
	if Integer >= 0 andalso Integer =< 16#7fffffffffffffff ->
			?encode_varint_field(FieldID, Integer);
		Integer >= -16#8000000000000000 andalso Integer < 0 ->
			encode_internal(uint64, FieldID, Integer + (1 bsl 64));
		true ->
			exit({error, {encode_internal, int64, FieldID, Integer}})
	end;
encode_internal(uint64, FieldID, Integer) ->
	case Integer band 16#ffffffffffffffff of
		Integer ->
			?encode_varint_field(FieldID, Integer);
		_ ->
			exit({error, {encode_internal, uint64, FieldID, Integer}})
	end;
encode_internal(bool, FieldID, Integer) ->
	case Integer band 1 =:= 1 of
		true ->
			?encode_varint_field(FieldID, Integer);
		_ ->
			exit({error, {encode_internal, bool, FieldID, Integer}})
	end;
encode_internal(sint32, FieldID, Integer)  ->
	if Integer >= 0 andalso Integer =< 16#7fffffff ->
			?encode_varint_field(FieldID, Integer bsl 1);
		Integer >= -16#80000000 andalso Integer < 0 ->
			?encode_varint_field(FieldID, bnot (Integer bsl 1));
		true ->
			exit({error, {encode_internal, sint32, FieldID, Integer}})
	end;
encode_internal(sint64, FieldID, Integer)  ->
	if Integer >= 0 andalso Integer =< 16#7fffffffffffffff ->
			?encode_varint_field(FieldID, Integer bsl 1);
		Integer >= -16#8000000000000000 andalso Integer < 0 ->
			?encode_varint_field(FieldID, bnot (Integer bsl 1));
		true ->
			exit({error, {encode_internal, sint64, FieldID, Integer}})
	end;    
encode_internal(fixed32, FieldID, Integer) ->
	case Integer band 16#ffffffff of
		Integer ->
			[?encode_field_tag(FieldID, ?TYPE_32BIT), << Integer:32/little-integer>>];
		_ ->
			exit({error, {encode_internal, fixed32, FieldID, Integer}})
	end;
encode_internal(sfixed32, FieldID, Integer)  ->
	if Integer >= -16#80000000 andalso Integer =< 16#7fffffff ->
			[?encode_field_tag(FieldID, ?TYPE_32BIT), <<Integer:32/little-integer>>];
		true ->
			exit({error, {encode_internal, sfixed32, FieldID, Integer}})
	end;
encode_internal(fixed64, FieldID, Integer)  ->
	case Integer band 16#ffffffffffffffff of
		Integer ->
			[?encode_field_tag(FieldID, ?TYPE_64BIT),  << Integer:64/little-integer>>];
		_ ->
			exit({error, {encode_internal, fixed64, FieldID, Integer}})
	end;
encode_internal(sfixed64, FieldID, Integer) ->
	if Integer >= -16#8000000000000000 andalso Integer =< 16#7fffffffffffffff ->
			[?encode_field_tag(FieldID, ?TYPE_64BIT),  <<Integer:64/little-integer>>];
		true ->
			exit({error, {encode_internal, sfixed64, FieldID, Integer}})
	end;
encode_internal(float, FieldID, Float) ->
	?encode_internal_float(FieldID, Float, ?TYPE_32BIT, 32);
encode_internal(double, FieldID, Float) ->
	?encode_internal_float(FieldID, Float, ?TYPE_64BIT, 64).
	

read_field_num_and_wire_type(Bytes) ->
    {Tag, Rest} = decode_varint(Bytes),
    FieldID = Tag bsr 3,
    WireType = Tag band 7,
    {{FieldID, WireType}, Rest}.
    
%% @spec decode(Bytes, ExpectedType) -> Result
%%       Bytes = binary()
%%       ExpectedType = bool | enum | int32 | uint32 | int64 | unit64 | sint32 | sint64 | fixed32 | sfixed32 | fixed64 | sfixed64 | string | bytes | float | double 
%%       Result = {{integer(), any()}, binary()}
decode(Bytes, ExpectedType) ->
    {{FieldID, WireType}, Rest} = read_field_num_and_wire_type(Bytes),
    {Value, Rest1} = decode_value(WireType, ExpectedType, Rest),
    {{FieldID, Value}, Rest1}.


%% @hidden
decode_value(?TYPE_VARINT, ExpectedType, Bytes) ->
    {Value, Rest} = decode_varint(Bytes),
    {typecast(ExpectedType, Value), Rest};
decode_value(?TYPE_STRING, bytes, Bytes) ->
    {Length, Rest} = decode_varint(Bytes),
    split_binary(Rest, Length);
decode_value(?TYPE_STRING, string, Bytes) ->
    {Length, Rest} = decode_varint(Bytes),
    split_binary(Rest, Length);
decode_value(?TYPE_64BIT, fixed64, <<Value:64/little-unsigned-integer, Rest/binary>>) ->
    {Value, Rest};
decode_value(?TYPE_64BIT, fixed32, <<Value:32/little-unsigned-integer, _:32, Rest/binary>>) ->
    {Value, Rest};
decode_value(?TYPE_64BIT, sfixed64, <<Value:64/little-signed-integer, Rest/binary>>) ->
    {Value, Rest};
decode_value(?TYPE_64BIT, sfixed32, <<Value:32/little-signed-integer, _:32, Rest/binary>>) ->
    {Value, Rest};
decode_value(?TYPE_32BIT, fixed32, <<Value:32/little-unsigned-integer, Rest/binary>>)  ->
    {Value, Rest};
decode_value(?TYPE_32BIT, fixed64, <<Value:32/little-unsigned-integer, Rest/binary>>) ->
    {Value, Rest};
decode_value(?TYPE_32BIT, sfixed32, <<Value:32/little-signed-integer, Rest/binary>>) ->
    {Value, Rest};
decode_value(?TYPE_32BIT, sfixed64, <<Value:32/little-signed-integer, Rest/binary>>) ->
    {Value, Rest};
decode_value(?TYPE_32BIT, float, <<Value:32/little-float, Rest/binary>>) ->
    {Value + 0.0, Rest};
decode_value(?TYPE_64BIT, double, <<Value:64/little-float, Rest/binary>>) ->
    {Value + 0.0, Rest};
decode_value(WireType, ExpectedType, _) ->
    exit({error, {unexpected_value, WireType, ExpectedType}}).

%% @hidden
-define(TYPECAST_INT(Value),
    if Value band 16#8000000000000000 =/= 0 -> Value - 16#10000000000000000;
        true -> Value
    end).
%% @hidden
-define(TYPECAST_SINT(Value),
    (Value bsr 1) bxor (-(Value band 1))).

%% @hidden
typecast(int32, Value) ->
    ?TYPECAST_INT(Value);
typecast(int64, Value) ->
    ?TYPECAST_INT(Value);
typecast(sint32, Value) ->
    ?TYPECAST_SINT(Value);
typecast(sint64, Value) ->
    ?TYPECAST_SINT(Value);
typecast(bool, Value) ->
    case Value of
      1 -> true;
      _ -> false
    end;
typecast(_, Value) ->
    Value.

%% @hidden
%%encode_field_tag(FieldID, FieldType) when FieldID band 16#3fffffff =:= FieldID ->
%%    encode_varint((FieldID bsl 3) bor FieldType).

%% @hidden
%%encode_varint_field(FieldID, Integer) ->
%%    [encode_field_tag(FieldID, ?TYPE_VARINT), encode_varint(Integer)].

%% @hidden
encode_varint(I) ->
    encode_varint(I, []).

%% @hidden
encode_varint(I, Acc) when I =< 16#7f ->
    iolist_to_binary(lists:reverse([I | Acc]));
encode_varint(I, Acc) ->
    Last_Seven_Bits = (I - ((I bsr 7) bsl 7)),
    First_X_Bits = (I bsr 7),
    With_Leading_Bit = Last_Seven_Bits bor 16#80,
    encode_varint(First_X_Bits, [With_Leading_Bit|Acc]).

%% @hidden
decode_varint(Bytes) ->
    decode_varint(Bytes, []).
decode_varint(<<0:1, I:7, Rest/binary>>, Acc) ->
    Result = decode_varint1([I|Acc], 0),
    {Result, Rest};
decode_varint(<<1:1, I:7, Rest/binary>>, Acc) ->
    decode_varint(Rest, [I | Acc]).

%% @hidden
%% decode_varint1(L) ->
%%     decode_varint1(L, 0).
decode_varint1([], Acc) ->
    Acc;
decode_varint1([X|R], Acc) ->
    decode_varint1(R, Acc bsl 7 bor X).
