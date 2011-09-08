%%% @author David Åberg <dag@david-ab-latitude>
%%% @copyright (C) 2011, David Åberg
%%% @doc
%%%
%%% @end
%%% Created :  6 Sep 2011 by David Åberg <dag@david-ab-latitude>
-module(proper_protobuffs).

-include_lib("proper/include/proper.hrl").

-export([prop_protobuffs/0,prop_protobuffs_packed/0, test_file/1]).

prop_protobuffs() ->
    ?FORALL({FieldID,{Value,Type}},{choose(0,16#3fffffff),value()},
	    begin
		case Type of
		    float ->
			Encoded = protobuffs:encode(FieldID,Value,Type),
			{{FieldID,Float},<<>>} = protobuffs:decode(Encoded,Type),
			fuzzy_match(Float,Value,3);
		    _Else ->
			Encoded = protobuffs:encode(FieldID,Value,Type),
			{{FieldID,Value},<<>>} == protobuffs:decode(Encoded,Type)
		end
	    end).

prop_protobuffs_packed() ->
    ?FORALL({FieldID,{Values,Type}},{choose(0,16#3fffffff),
				     oneof([{non_empty(list(uint32())),uint32},
					    {non_empty(list(uint64())),uint64},
					    {non_empty(list(sint32())),sint32},
					    {non_empty(list(sint64())),sint64},
					    {non_empty(list(sint32())),int32},
					    {non_empty(list(sint64())),int64},
					    {non_empty(list(bool())),bool},
					    {non_empty(list(real())),double},
					    {non_empty(list(real())),float}])},
	    begin
		case Type of
		    float ->
			Encoded = protobuffs:encode_packed(FieldID,Values,Type),
			{{FieldID,DecodedValues},<<>>} = protobuffs:decode_packed(Encoded,Type),
			lists:all(fun({Expected,Result}) -> fuzzy_match(Expected,Result,3) end, lists:zip(Values,DecodedValues));
		    _Else ->
			Encoded = protobuffs:encode_packed(FieldID,Values,Type),
			Decoded = protobuffs:decode_packed(Encoded,Type),
			{{FieldID,Values},<<>>} == Decoded
		end
	    end).

proper_test_protobuffs() ->
    proper:check_specs(protpbuffs).

test_file(File) ->
    Expected = {empty,default(undefined, real()),
		default(undefined, real()),
		default(undefined, sint32()),
		default(undefined, sint64()),
		default(undefined, uint32()),
		default(undefined, uint64()),
		default(undefined, sint32()),
		default(undefined, sint64()),
		default(undefined, uint32()),
		default(undefined, uint64()),
		default(undefined, sint32()),
		default(undefined, sint64()),
		default(undefined, bool()),
		default(undefined, string()),
		default(undefined, binary()),
		default(undefined, {empty_emptymessage})},

    error_logger:info_msg("testing ~p",[File]),
    protobuffs_compile:scan_file(File),
    Name = filename:basename(File, ".proto"),
    NamePB = list_to_atom(Name ++"_pb"),
    NameDecode = list_to_atom("decode_"++Name),
    NameEncode = list_to_atom("encode_"++Name), 
    ?FORALL({Message},
	    {Expected},
	    begin
		Decoded = NamePB:NameDecode(NamePB:NameEncode(Message)),
		compare_messages(Message,Decoded)
	    end).

compare_messages(ExpectedMsg,Msg) ->
    lists:foldl(		
      fun({E,D}, Acc) -> compare(E,D) andalso Acc end, 
      true, 	
      lists:zip(tuple_to_list(ExpectedMsg),tuple_to_list(Msg))).

compare(A,A) ->
    true;
compare([A],B) ->
    compare(A,B);
compare(A,[B]) ->
    compare(A,B);
compare(A,B) when is_tuple(A), is_tuple(B) ->
    compare(tuple_to_list(A),tuple_to_list(B));
compare([A|RA],[B|RB]) ->
    compare(A,B) andalso compare(RA,RB);
compare(A,B) when is_float(A),is_float(B) ->
    fuzzy_match(A,B,3);
compare(_,undefined) ->
    true;
compare(undefined,_) ->
    true;
compare(_,_) ->
    false.

fuzzy_match(A,A,_) ->
    true;
fuzzy_match(A,B,L) ->
    <<AT:L/binary, _/binary>> = <<A/float>>,
    <<BT:L/binary, _/binary>> = <<B/float>>,
    AT == BT.

uint32() ->
    choose(0, 16#ffffffff).

sint32() ->
    choose(-16#80000000, 16#7fffffff).

uint64() ->
    choose(0,16#ffffffffffffffff).

sint64() ->
    choose(-16#8000000000000000,16#7fffffffffffffff).

value() ->
    oneof([{real(),double},
	   {real(),float},
	   {nan,float},
	   {infinity,float},
	   {'-infinity',float},
	   {nan,double},
	   {infinity,double},
	   {'-infinity',double},
	   {uint32(),uint32},
	   {uint64(),uint64},
	   {sint32(),sint32},
	   {sint64(),sint64},
	   {uint32(),fixed32},
	   {uint64(),fixed64},
	   {sint32(),sfixed32},
	   {sint64(),sfixed64},
	   {sint32(),int32},
	   {sint64(),int64},
	   {bool(),bool},
	   {sint32(),enum},
	   {string(),string},
	   {binary(),bytes}]).
