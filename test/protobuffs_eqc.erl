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
    ?FORALL({Real1, Float1, Int1, 
	     Int2, Int3, Int4, 
	     Int5, Int6, Int7, 
	     Int8, Int9, Int10, 
	     Val1, Str1, Bit1},
	    {default(undefined, real()),
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
	     default(undefined, binary())},
	    begin
		{empty,Real1,Float11,
		 Int1,Int2,Int3,Int4,Int5,
		 Int6,Int7,Int8,Int9,Int10,
		 Val1,Str1,Bit1} = empty_pb:decode_empty(
				     empty_pb:encode_empty({empty,
							    Real1,Float1,
							    Int1,Int2,Int3,Int4,Int5,
							    Int6,Int7,Int8,Int9,Int10,
							    Val1,Str1,Bit1})),
		fuzzy_match(Float1,Float11,3)
	    end).

check_with_default(Expected,Result,undefined,Fun) ->
    Fun(Expected,Result);
check_with_default(undefined,Result,Default,Fun) ->
    Fun(Default,Result);
check_with_default(Expected,Result,_Default,Fun) ->
    Fun(Expected,Result).

prop_protobuffs_has_default() ->
    ?FORALL({Real1, Float1, Int1,
	     Int2, Int3, Int4,
	     Int5, Int6, Int7,
	     Int8, Int9, Int10,
	     Val1, Str1
	    },
	    {default(undefined, real()),
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
	     default(undefined, string())},
	    begin
		{withdefault,Real11,Float11,
		 Int11,Int12,Int13,Int14,Int15,Int16,
		 Int17,Int18,Int19,Int110,
		 Val11,Str11} = hasdefault_pb:decode_withdefault(
				  hasdefault_pb:encode_withdefault(
				    {withdefault,
				     Real1,Float1,
				     Int1,Int2,Int3,Int4,Int5,Int6,
				     Int7,Int8,Int9,Int10,
				     Val1,Str1})),
		check_with_default(
		  Real1,Real11,1.0,
		  fun(Expected,Result) -> Expected == Result end), 
		check_with_default(
		  Float1,Float11,2.0,
		  fun(Expected,Result) -> fuzzy_match(Expected,Result,3) end),
		check_with_default(
		  Int1,Int11,1,
		  fun(Expected,Result) -> Expected == Result end),
		check_with_default(
		  Int2,Int12,2,
		  fun(Expected,Result) -> Expected == Result end),
		check_with_default(
		  Int3,Int13,3,
		  fun(Expected,Result) -> Expected == Result end),
		check_with_default(
		  Int4,Int14,4,
		  fun(Expected,Result) -> Expected == Result end),
		check_with_default(
		  Int5,Int15,5,
		  fun(Expected,Result) -> Expected == Result end),
		check_with_default(
		  Int6,Int16,6,
		  fun(Expected,Result) -> Expected == Result end),
		check_with_default(
		  Int7,Int17,7,
		  fun(Expected,Result) -> Expected == Result end),
		check_with_default(
		  Int8,Int18,8,
		  fun(Expected,Result) -> Expected == Result end),
		check_with_default(
		  Int9,Int19,9,
		  fun(Expected,Result) -> Expected == Result end),
		check_with_default(
		  Int10,Int110,10,
		  fun(Expected,Result) -> Expected == Result end),
		check_with_default(
		  Val1,Val11,true,
		  fun(Expected,Result) -> Expected == Result end),
		check_with_default(
		  Str1,Str11,"test",
		  fun(Expected,Result) -> Expected == Result end)
	    end).

location() ->
    Str = string(),
    default(undefined,{location,Str,Str}).

prop_protobuffs_simple() ->
    ?FORALL({Name, Address, PhoneNumber,Age,Location},
	    {string(),string(),string(),sint32(),location()},
	    begin
		Msg = {person,Name,Address,PhoneNumber,Age,Location},
		Msg == simple_pb:decode_person(simple_pb:encode_person(Msg))
	    end).

phone_type() ->
    Int32 = default(undefined,sint32()),
    {person_phonenumber_phonetype,Int32,Int32,Int32}.

phone_number() ->
    list({person_phonenumber,string(),default(undefined,phone_type())}).

prop_protobuffs_nested1() ->
    ?FORALL({Name, Id, Email, PhoneNumber},
	    {string(),sint32(),default(undefined,string()),phone_number()},
	    begin
		Msg = {person,Name,Id,Email,PhoneNumber},
		case nested1_pb:decode_person(nested1_pb:encode_person(Msg)) of
		    {person,Name,Id,Email,PhoneNumber} -> true;
		    {person,Name,Id,Email,undefined} ->
			PhoneNumber == [];
		    _Else ->
			false
		end
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
    ?FORALL({MiddleAA, MiddleBB},
	    {default(undefined,middleAA()),default(undefined,middleBB())},
	    begin
		Msg = {outer,MiddleAA,MiddleBB},
		Msg == nested2_pb:decode_outer(nested2_pb:encode_outer(Msg))
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
	    {default(undefined,middle())},
	    begin
		Msg = {outer,Middle},
		Msg == nested3_pb:decode_outer(nested3_pb:encode_outer(Msg))
	    end).

prop_protobuffs_nested4() ->
    ?FORALL({Middle},
	    {default(undefined,middle())},
	    begin
		Msg = {outer,Middle},
		Msg == nested4_pb:decode_outer(nested4_pb:encode_outer(Msg))
	    end).

first_inner() ->
    {first_inner,default(undefined,bool())}.

prop_protobuffs_nested5_1() ->
    ?FORALL({Inner},
	    {default(undefined,first_inner())},
	    begin
		Msg = {first,Inner},
		Msg == nested5_pb:decode_first(nested5_pb:encode_first(Msg))
	    end).

prop_protobuffs_nested5_2() ->
    ?FORALL({Inner},
	    {first_inner()},
	    begin
		Msg = {second,Inner},
		Msg == nested5_pb:decode_second(nested5_pb:encode_second(Msg))
	    end).

enum_value() ->
    oneof([value1,value2]).

prop_protobuffs_enum() ->
    ?FORALL({Middle},
	    {default(undefined,enum_value())},
	    begin
		Msg = {enummsg,Middle},
		Msg == enum_pb:decode_enummsg(enum_pb:encode_enummsg(Msg))
	    end).

address_phone_number() ->
    list({person_phonenumber,string(),default(undefined,oneof(['HOME','WORK','MOBILE']))}).
    
addressbook() ->
    list({person,string(),sint32(),string(),default(undefined,address_phone_number())}).

prop_protobuffs_addressbook() ->
    ?FORALL({Addressbook},
	    {default(undefined,addressbook())},
	    begin
		Msg = {addressbook,Addressbook},
		addressbook_pb:decode_addressbook(addressbook_pb:encode_addressbook(Msg)),
		true
	    end).

repeater_location() ->
    {location,string(),string()}.

repeater_person() ->
    {person,string(),string(),string(),sint32(),default(undefined,list(string())),default(undefined,list(repeater_location())),list(uint32())}.

prop_protobuffs_repeater() ->
    ?FORALL({Repeater},
	    {repeater_person()},
	    begin
		repeater_pb:decode_person(repeater_pb:encode_person(Repeater)),
		true
	    end).

prop_protobuffs_packed_repeated() ->
    ?FORALL({Repeater},
	    {repeater_person()},
	    begin
		packed_repeated_pb:decode_person(packed_repeated_pb:encode_person(Repeater)),
		true
	    end).

special_words() ->
    {message,
     string(),string(),string(),string(),string(),
     string(),string(),string(),string(),string(),
     string(),string(),string(),string(),string(),
     string(),string(),string(),string(),string(),
     string(),string(),string(),string()}.

prop_protobuffs_special_words() ->
    ?FORALL({SpecialWords},
	    {special_words()},
	    begin
		Decoded = special_words_pb:decode_message(special_words_pb:encode_message(SpecialWords)),
		lists:foldl(
		  fun({E,D}, Acc) -> E =:= D andalso Acc end, 
		  true, 
		  lists:zip(tuple_to_list(SpecialWords),tuple_to_list(Decoded)))
	    end).
