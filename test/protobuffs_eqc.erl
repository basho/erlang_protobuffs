%%%-------------------------------------------------------------------
%%% File    : protobuffs_eqc.erl
%%% Author  : David AAberg <david_ab@RB-DAVIDAB01>
%%% Description : 
%%%
%%% Created :  5 Aug 2010 by David AAberg <david_ab@RB-DAVIDAB01>
%%%-------------------------------------------------------------------
-module(protobuffs_eqc).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

uint32() ->
    choose(0, 16#ffffffff).

sint32() ->
    choose(-16#80000000, 16#7fffffff).

uint64() ->
    choose(0,16#ffffffffffffffff).

sint64() ->
    choose(-16#8000000000000000,16#7fffffffffffffff).

string() ->
    non_empty(list(char())).

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
	   {uint32(),enum},
	   {string(),string},
	   {binary(),bytes}]).

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

prop_protobuffs_packed() ->
    ?FORALL({FieldID,{Values,Type}},{?SUCHTHAT(I, uint32(),I =< 16#3fffffff ),oneof([{non_empty(list(uint32())),uint32},
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

prop_protobuffs_empty() ->
    ?FORALL({Empty},
	    {{empty,default(undefined, real()),
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
		    default(undefined, binary())}},
	    begin
		Decoded = empty_pb:decode_empty(empty_pb:encode_empty(Empty)),
		compare_messages(Empty,Decoded)
	    end).

check_with_default(Expected,Result,undefined,Fun) ->
    Fun(Expected,Result);
check_with_default(undefined,Result,Default,Fun) ->
    Fun(Default,Result);
check_with_default(Expected,Result,_Default,Fun) ->
    Fun(Expected,Result).

prop_protobuffs_has_default() ->
    ?FORALL({Withdefault},
	    {{withdefault,default(undefined, real()),
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
	     default(undefined, string())}},
	    begin
		Decoded = hasdefault_pb:decode_withdefault(hasdefault_pb:encode_withdefault(Withdefault)),
		compare_messages(Withdefault,Decoded)
	    end).

location() ->
    Str = string(),
    default(undefined,{location,Str,Str}).

prop_protobuffs_simple() ->
    ?FORALL({Person},
	    {{person,string(),string(),string(),sint32(),location()}},
	    begin
		Decoded = simple_pb:decode_person(simple_pb:encode_person(Person)),
		compare_messages(Person,Decoded)
	    end).

phone_type() ->
    Int32 = default(undefined,sint32()),
    {person_phonenumber_phonetype,Int32,Int32,Int32}.

phone_number() ->
    list({person_phonenumber,string(),default(undefined,phone_type())}).

prop_protobuffs_nested1() ->
    ?FORALL({Person},
	    {{person,string(),sint32(),default(undefined,string()),phone_number()}},
	    begin
		Decoded = nested1_pb:decode_person(nested1_pb:encode_person(Person)),
		compare_messages(Person,Decoded)
	    end).

innerAA() ->
    {outer_middleaa_inner,sint64(),default(undefined,bool())}.


middleAA() ->
    Inner = innerAA(),
    {outer_middleaa,default(undefined,Inner)}.

innerBB() ->
    {outer_middlebb_inner,sint32(),default(undefined,bool())}.

middleBB() ->
    Inner = innerBB(),
    {outer_middlebb,default(undefined,Inner)}.

prop_protobuffs_nested2() ->
    ?FORALL({Middle},
	    {{outer,default(undefined,middleAA()),default(undefined,middleBB())}},
	    begin
		Decoded = nested2_pb:decode_outer(nested2_pb:encode_outer(Middle)),
		compare_messages(Middle,Decoded)
	    end).

inner() ->
    {outer_middle_inner,default(undefined,bool())}.

other() ->
    {outer_other,default(undefined,bool())}.

middle() ->
    Inner = inner(),
    Other = other(),
    {outer_middle,Inner,Other}.


prop_protobuffs_nested3() ->
    ?FORALL({Middle},
	    {default({outer,undefined},{outer,middle()})},
	    begin
		Decoded = nested3_pb:decode_outer(nested3_pb:encode_outer(Middle)),
		compare_messages(Middle,Decoded)
	    end).

prop_protobuffs_nested4() ->
    ?FORALL({Middle},
	    {default({outer,undefined},{outer,middle()})},
	    begin
		Decoded = nested4_pb:decode_outer(nested4_pb:encode_outer(Middle)),
		compare_messages(Middle,Decoded)
	    end).

first_inner() ->
    {first_inner,default(undefined,bool())}.

prop_protobuffs_nested5_1() ->
    ?FORALL({Inner},
	    {default({first,undefined},{first,first_inner()})},
	    begin
		Decoded = nested5_pb:decode_first(nested5_pb:encode_first(Inner)),
		compare_messages(Inner,Decoded)
	    end).

prop_protobuffs_nested5_2() ->
    ?FORALL({Inner},
	    {{second,first_inner()}},
	    begin
		Decoded = nested5_pb:decode_second(nested5_pb:encode_second(Inner)),
		compare_messages(Inner,Decoded)
	    end).

enum_value() ->
    oneof([value1,value2]).

prop_protobuffs_enum() ->
    ?FORALL({Middle},
	    {default({enummsg,undefined},{enummsg,enum_value()})},
	    begin
		Decoded = enum_pb:decode_enummsg(enum_pb:encode_enummsg(Middle)),
		compare_messages(Middle,Decoded)
	    end).

enum_outside_value() ->
	oneof(['FIRST','SECOND']).

prop_protobuffs_enum_outside() ->
	?FORALL({Middle},
		{default({enumuser,undefined},{enumuser,enum_outside_value()})},
		begin
		Decoded = enum_outside_pb:decode_enumuser(enum_outside_pb:encode_enumuser(Middle)),
		compare_messages(Middle,Decoded)
		end).

prop_protobuffs_extentions() ->
	?FORALL({Middle},
		{default({extendable},{maxtendable})},
		begin
		DecodeFunc = list_to_atom("decode_" ++ atom_to_list(element(1, Middle))),
		Decoded = extensions_pb:DecodeFunc(extensions_pb:encode(Middle)),
		compare_messages(Middle,Decoded)
		end).

address_phone_number() ->
    list({person_phonenumber,string(),default(undefined,oneof(['HOME','WORK','MOBILE']))}).
    
addressbook() ->
    list({person,string(),sint32(),string(),default(undefined,address_phone_number())}).

prop_protobuffs_addressbook() ->
    ?FORALL({Addressbook},
	    {default({addressbook,undefined},{addressbook,addressbook()})},
	    begin
		Decoded = addressbook_pb:decode_addressbook(addressbook_pb:encode_addressbook(Addressbook)),
		compare_messages(Addressbook,Decoded)
	    end).

repeater_location() ->
    {location,string(),string()}.

repeater_person() ->
    {person,string(),string(),string(),sint32(),default(undefined,list(string())),default(undefined,list(repeater_location())),list(uint32())}.

prop_protobuffs_repeater() ->
    ?FORALL({Repeater},
	    {repeater_person()},
	    begin
		Decoded = repeater_pb:decode_person(repeater_pb:encode_person(Repeater)),
		compare_messages(Repeater,Decoded)
	    end).

prop_protobuffs_packed_repeated() ->
    ?FORALL({Repeater},
	    {repeater_person()},
	    begin
		Decoded = packed_repeated_pb:decode_person(packed_repeated_pb:encode_person(Repeater)),
		compare_messages(Repeater,Decoded)
	    end).

special_words() ->
    {message,
     string(),string(),string(),string(),string(),
     string(),string(),string(),string(),string(),
     string(),string(),string(),string(),string(),
     string(),string(),string(),string(),string(),
     string(),string(),string(),string(),string(),
     string(),string(),string(),string(),string()}.

prop_protobuffs_special_words() ->
    ?FORALL({SpecialWords},
	    {special_words()},
	    begin
		Decoded = special_words_pb:decode_message(special_words_pb:encode_message(SpecialWords)),
		compare_messages(SpecialWords,Decoded)
	    end).
