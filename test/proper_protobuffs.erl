%%% @author David Åberg <dag@david-ab-latitude>
%%% @copyright (C) 2011, David Åberg
%%% @doc
%%%
%%% @end
%%% Created :  6 Sep 2011 by David Åberg <dag@david-ab-latitude>
-module(proper_protobuffs).

-include_lib("proper/include/proper.hrl").

-export([prop_protobuffs/0]).

prop_protobuffs() ->
    ?FORALL({FieldID,{Value,Type}},{?SUCHTHAT(I, uint32(),I =< 16#3fffffff ),value()},
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
	   {list(byte()),string},
	   {binary(),bytes}]).
