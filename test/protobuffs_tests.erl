%%%-------------------------------------------------------------------
%%% File    : protobuffs_tests.erl
%%% Author  : David AAberg <david_ab@RB-DAVIDAB01>
%%% Description : 
%%%
%%% Created :  2 Aug 2010 by David AAberg <david_ab@RB-DAVIDAB01>
%%%-------------------------------------------------------------------
-module(protobuffs_tests).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% Encode/Decode int32 value 150
%%--------------------------------------------------------------------
encode_test1_test_() ->
    [?_assertMatch(<<8,150,1>>,protobuffs:encode(1, 150, int32))].
decode_test1_test_() ->
    [?_assertMatch({{1, 150},<<>>}, protobuffs:decode(<<8,150,1>>, int32))].

%%--------------------------------------------------------------------
%% Encode/Decode string "testing"
%%--------------------------------------------------------------------
encode_test2_test_() ->
    [?_assertMatch(<<18,7,116,101,115,116,105,110,103>>,protobuffs:encode(2,"testing",string))].
decode_test2_test_() ->
    [?_assertMatch({{2,"testing"},<<>>},protobuffs:decode(<<18,7,116,101,115,116,105,110,103>>,string))].

%%--------------------------------------------------------------------
%% Encode/Decode Test1
%%--------------------------------------------------------------------
encode_test3_test_() ->
    [?_assertMatch(<<26,3,8,150,1>>,protobuffs:encode(3,<<8,150,1>>,bytes))].
decode_test3_test_() ->
    [?_assertMatch({{3,<<8,150,1>>},<<>>},protobuffs:decode(<<26,3,8,150,1>>,bytes))].

%%--------------------------------------------------------------------
%% Encode/Decode repeated
%%--------------------------------------------------------------------
encode_test4_test_() ->
    [?_assertMatch(<<34,6,3,142,2,158,167,5>>,protobuffs:encode_packed(4,[3,270,86942],int32))].
decode_test4_test_() ->
    [?_assertMatch({{4,[3,270,86942]},<<>>},protobuffs:decode_packed(<<34,6,3,142,2,158,167,5>>,int32))].

parse_empty_file_test_() ->
    Path = filename:absname("../test/erlang_protobuffs_SUITE_data/empty.proto"),
    io:format("Test path ~p~n",[Path]),
    [{message, "Empty", Messages}] = parse(Path),
    [?_assertMatch({1,optional,"double","real1",number,none},lists:keyfind(1,1,Messages)),
     ?_assertMatch({2,optional,"float","real2",number,none},lists:keyfind(2,1,Messages)),
     ?_assertMatch({3,optional,"int32","int1",number,none},lists:keyfind(3,1,Messages)),
     ?_assertMatch({4,optional,"int64","int2",number,none},lists:keyfind(4,1,Messages)),
     ?_assertMatch({5,optional,"uint32","int3",number,none},lists:keyfind(5,1,Messages)),
     ?_assertMatch({6,optional,"uint64","int4",number,none},lists:keyfind(6,1,Messages)),
     ?_assertMatch({7,optional,"sint32","int5",number,none},lists:keyfind(7,1,Messages)),
     ?_assertMatch({8,optional,"sint64","int6",number,none},lists:keyfind(8,1,Messages)),
     ?_assertMatch({9,optional,"fixed32","int7",number,none},lists:keyfind(9,1,Messages)),
     ?_assertMatch({10,optional,"fixed64","int8",number,none},lists:keyfind(10,1,Messages)),
     ?_assertMatch({11,optional,"sfixed32","int9",number,none},lists:keyfind(11,1,Messages)),
     ?_assertMatch({12,optional,"sfixed64","int10",number,none},lists:keyfind(12,1,Messages)),
     ?_assertMatch({13,optional,"bool","val1",number,none},lists:keyfind(13,1,Messages)),
     ?_assertMatch({14,optional,"string","str1",number,none},lists:keyfind(14,1,Messages)),
     ?_assertMatch({15,optional,"bytes","bit1",number,none},lists:keyfind(15,1,Messages))].

parse_has_default_test_() ->
    Path = filename:absname("../test/erlang_protobuffs_SUITE_data/hasdefault.proto"),
    [{message, "WithDefault", Messages}] = parse(Path),
    [?_assertMatch({1,required,"double","real1",number,1.0},lists:keyfind(1,1,Messages)),
     ?_assertMatch({2,required,"float","real2",number,2.0},lists:keyfind(2,1,Messages)),
     ?_assertMatch({3,required,"int32","int1",number,1},lists:keyfind(3,1,Messages)),
     ?_assertMatch({4,required,"int64","int2",number,2},lists:keyfind(4,1,Messages)),
     ?_assertMatch({5,required,"uint32","int3",number,3},lists:keyfind(5,1,Messages)),
     ?_assertMatch({6,required,"uint64","int4",number,4},lists:keyfind(6,1,Messages)),
     ?_assertMatch({7,required,"sint32","int5",number,5},lists:keyfind(7,1,Messages)),
     ?_assertMatch({8,required,"sint64","int6",number,6},lists:keyfind(8,1,Messages)),
     ?_assertMatch({9,required,"fixed32","int7",number,7},lists:keyfind(9,1,Messages)),
     ?_assertMatch({10,required,"fixed64","int8",number,8},lists:keyfind(10,1,Messages)),
     ?_assertMatch({11,required,"sfixed32","int9",number,9},lists:keyfind(11,1,Messages)),
     ?_assertMatch({12,required,"sfixed64","int10",number,10},lists:keyfind(12,1,Messages)),
     ?_assertMatch({13,required,"bool","val1",number,true},lists:keyfind(13,1,Messages)),
     ?_assertMatch({14,required,"string","str1",number,"test"},lists:keyfind(14,1,Messages))].

parse_simple_test_() ->
    Path = filename:absname("../test/erlang_protobuffs_SUITE_data/simple.proto"),
    [{package,"simple"},
     {message, "Person", Person},
     {message, "Location", Location}] = parse(Path),
    [?_assertMatch({1,required,"string","name",number,none},lists:keyfind(1,1,Person)),
     ?_assertMatch({2,required,"string","address",number,none},lists:keyfind(2,1,Person)),
     ?_assertMatch({3,required,"string","phone_number",number,none},lists:keyfind(3,1,Person)),
     ?_assertMatch({4,required,"int32","age",number,none},lists:keyfind(4,1,Person)),
     ?_assertMatch({5,optional,"Location","location",number,none},lists:keyfind(5,1,Person)),
     ?_assertMatch({1,required,"string","region",number,none},lists:keyfind(1,1,Location)),
     ?_assertMatch({2,required,"string","country",number,none},lists:keyfind(2,1,Location))].

parse_enum_test_() ->
    Path = filename:absname("../test/erlang_protobuffs_SUITE_data/enum.proto"),
    [{message,"EnumMsg", EnumMsg}] = parse(Path),
    {enum, "Values", Values} = lists:keyfind(enum,1,EnumMsg),
    [?_assertMatch({1,optional,"Values","value",number,none},lists:keyfind(1,1,EnumMsg)),
     ?_assertMatch({enum,1,"value1"},lists:keyfind("value1",3,Values)),
     ?_assertMatch({enum,2,"value2"},lists:keyfind("value2",3,Values))].

parse_enum_outside_test() ->
	Path = filename:absname("../test/erlang_protobuffs_SUITE_data/enum_outside.proto"),
	[{enum, "EnumList", Enums}, {message, "EnumUser", EnumUser}] = parse(Path),
	[?_assertMatch({1,optional,"EnumList", "enum_filed", number,none},lists:keyfind(1,1,EnumUser)),
	?_assertMatch({enum,1,"FIRST"},lists:keyfind("FIRST",3,Enums)),
	?_assertMatch({enum,2,"SECOND"},lists:keyfind("SECOND",3,Enums))].

parse_extensions_test() ->
	Path = filename:absname("../test/erlang_protobuffs_SUITE_data/extensions.proto"),
	[{message, "Extendable", Extendable}, {message, "MaxTendable", MaxTendable}] = parse(Path),
	[?_assertMatch({extensions, 100, 200}, lists:nth(1, Extendable)),
	?_assertMatch({extensions, 100, max}, lists:nth(1, MaxTendable))].

parse_service_test() ->
	Path = filename:absname("../test/erlang_protobuffs_SUITE_data/service.proto"),
	[{service, "SearchService", [SearchService]}, _, _] = parse(Path),
	[?_assertMatch({rpc, "Search", "SearchRequest", "SearchResponse"},SearchService)].

parse_nested1_test_() ->
    Path = filename:absname("../test/erlang_protobuffs_SUITE_data/nested1.proto"),
    [{message, "Person", Person}] = parse(Path),
    {message, "PhoneNumber", PhoneNumber} = lists:keyfind("PhoneNumber",2,Person),
    {message, "PhoneType", PhoneType} = lists:keyfind("PhoneType",2,PhoneNumber),
    [?_assertMatch({1,required,"string","name",number,none},lists:keyfind(1,1,Person)),
     ?_assertMatch({2,required,"int32","id",number,none},lists:keyfind(2,1,Person)),
     ?_assertMatch({3,optional,"string","email",number,none},lists:keyfind(3,1,Person)),
     ?_assertMatch({4,repeated,"PhoneNumber","phone",number,none},lists:keyfind(4,1,Person)),
     ?_assertMatch({1,required,"string","number",number,none},lists:keyfind(1,1,PhoneNumber)),
     ?_assertMatch({1,optional,"int32","mobile",number,none},lists:keyfind(1,1,PhoneType)),
     ?_assertMatch({2,optional,"int32","home",number,none},lists:keyfind(2,1,PhoneType)),
     ?_assertMatch({3,optional,"int32","work",number,none},lists:keyfind(3,1,PhoneType))].

parse_nested2_test_() ->
    Path = filename:absname("../test/erlang_protobuffs_SUITE_data/nested2.proto"),
    [{message, "Outer",Outer}] = parse(Path),
    {message, "MiddleAA", MiddleAA} = lists:keyfind("MiddleAA",2,Outer),
    {message, "MiddleBB", MiddleBB} = lists:keyfind("MiddleBB",2,Outer),
    {message, "Inner", InnerAA} = lists:keyfind("Inner",2,MiddleAA),
    {message, "Inner", InnerBB} = lists:keyfind("Inner",2,MiddleBB),
    [?_assertMatch({1,optional,"MiddleAA","middleaa",number,none},lists:keyfind(1,1,Outer)),
     ?_assertMatch({2,optional,"MiddleBB","middlebb",number,none},lists:keyfind(2,1,Outer)),
     ?_assertMatch({1,optional,"Inner","inner",number,none},lists:keyfind(1,1,MiddleAA)),
     ?_assertMatch({1,optional,"Inner","inner",number,none},lists:keyfind(1,1,MiddleBB)),
     ?_assertMatch({1,required,"int64","ival",number,none},lists:keyfind(1,1,InnerAA)),
     ?_assertMatch({2,optional,"bool","booly",number,none},lists:keyfind(2,1,InnerAA)),
     ?_assertMatch({1,required,"int32","ival",number,none},lists:keyfind(1,1,InnerBB)),
     ?_assertMatch({2,optional,"bool","booly",number,none},lists:keyfind(2,1,InnerBB))].

parse_nested3_test_() ->
    Path = filename:absname("../test/erlang_protobuffs_SUITE_data/nested3.proto"),
    [{message,"Outer",Outer}] = parse(Path),
    {message,"Middle",Middle} = lists:keyfind("Middle",2,Outer),
    {message,"Other",Other} = lists:keyfind("Other",2,Outer),
    {message,"Inner",Inner} = lists:keyfind("Inner",2,Middle),
    [?_assertMatch({1,optional,"Middle","middle",number,none},lists:keyfind(1,1,Outer)),
     ?_assertMatch({2,optional,"Inner","inner",number,none},lists:keyfind(2,1,Middle)),
     ?_assertMatch({3,optional,"Other","other",number,none},lists:keyfind(3,1,Middle)),
     ?_assertMatch({1,optional,"bool","foo",number,none},lists:keyfind(1,1,Inner)),
     ?_assertMatch({1,optional,"bool","bar",number,none},lists:keyfind(1,1,Other))].

parse_nested4_test_() ->
    Path = filename:absname("../test/erlang_protobuffs_SUITE_data/nested4.proto"),
    [{message,"Outer",Outer}] = parse(Path),
    {message,"Middle",Middle} = lists:keyfind("Middle",2,Outer),
    {message,"Other",Other} = lists:keyfind("Other",2,Outer),
    {message,"Inner",Inner} = lists:keyfind("Inner",2,Middle),
    [?_assertMatch({1,optional,"Middle","middle",number,none},lists:keyfind(1,1,Outer)),
     ?_assertMatch({2,optional,"Inner","inner",number,none},lists:keyfind(2,1,Middle)),
     ?_assertMatch({3,optional,"Other","other",number,none},lists:keyfind(3,1,Middle)),
     ?_assertMatch({1,optional,"bool","foo",number,none},lists:keyfind(1,1,Inner)),
     ?_assertMatch({1,optional,"bool","bar",number,none},lists:keyfind(1,1,Other))].

parse_nested5_test_() ->
    Path = filename:absname("../test/erlang_protobuffs_SUITE_data/nested5.proto"),
    Parsed = parse(Path),
    {message,"First",First} = lists:keyfind("First",2,Parsed),
    {message,"Second",Second} = lists:keyfind("Second",2,Parsed),
    {message,"Inner",Inner} = lists:keyfind("Inner",2,First),
    [?_assertMatch({1,optional,"Inner","inner",number,none},lists:keyfind(1,1,First)),
     ?_assertMatch({1,optional,"bool","foo",number,none},lists:keyfind(1,1,Inner)),
     ?_assertMatch({1,required,"First.Inner","inner",number,none},lists:keyfind(1,1,Second))].

parse_addressbook_test_() ->
    Path = filename:absname("../test/erlang_protobuffs_SUITE_data/addressbook.proto"),
    Parsed = parse(Path),
    {message,"Person",Person} = lists:keyfind("Person",2,Parsed),
    {message,"PhoneNumber",PhoneNumber} = lists:keyfind("PhoneNumber",2,Person),
    {enum, "PhoneType", PhoneType} = lists:keyfind(enum,1,Person),
    {message,"AddressBook",AddressBook} = lists:keyfind("AddressBook",2,Parsed),
    [?_assertMatch({1,required,"string","name",number,none},lists:keyfind(1,1,Person)),
     ?_assertMatch({2,required,"int32","id",number,none},lists:keyfind(2,1,Person)),
     ?_assertMatch({3,optional,"string","email",number,none},lists:keyfind(3,1,Person)),
     ?_assertMatch({4,repeated,"PhoneNumber","phone",number,none},lists:keyfind(4,1,Person)),
     ?_assertMatch({1,required,"string","number",number,none},lists:keyfind(1,1,PhoneNumber)),
     ?_assertMatch({2,optional,"PhoneType","type",number,'HOME'},lists:keyfind(2,1,PhoneNumber)),
     ?_assertMatch({1,repeated,"Person","person",number,none},lists:keyfind(1,1,AddressBook)),
     ?_assertMatch({enum,0,"MOBILE"},lists:keyfind("MOBILE",3,PhoneType)),
     ?_assertMatch({enum,1,"HOME"},lists:keyfind("HOME",3,PhoneType)),
     ?_assertMatch({enum,2,"WORK"},lists:keyfind("WORK",3,PhoneType))].

parse_repeater_test_() ->
    Path = filename:absname("../test/erlang_protobuffs_SUITE_data/repeater.proto"),
    Parsed = parse(Path),
    {message,"Person",Person} = lists:keyfind("Person",2,Parsed),
    {message,"Location",Location} = lists:keyfind("Location",2,Parsed),
    [?_assertMatch({1,required,"string","name",number,none},lists:keyfind(1,1,Person)),
     ?_assertMatch({2,required,"string","address",number,none},lists:keyfind(2,1,Person)),
     ?_assertMatch({3,required,"string","phone_number",number,none},lists:keyfind(3,1,Person)),
     ?_assertMatch({4,required,"int32","age",number,none},lists:keyfind(4,1,Person)),
     ?_assertMatch({5,repeated,"string","hobbies",number,none},lists:keyfind(5,1,Person)),
     ?_assertMatch({6,repeated,"Location","locations",number,none},lists:keyfind(6,1,Person)),
     ?_assertMatch({7,repeated,"uint32","ids",number,none},lists:keyfind(7,1,Person)),
     ?_assertMatch({1,required,"string","region",number,none},lists:keyfind(1,1,Location)),
     ?_assertMatch({2,required,"string","country",number,none},lists:keyfind(2,1,Location))].

parse_packed_repeated_test_() ->
    Path = filename:absname("../test/erlang_protobuffs_SUITE_data/packed_repeated.proto"),
    Parsed = parse(Path),
    {message,"Person",Person} = lists:keyfind("Person",2,Parsed),
    {message,"Location",Location} = lists:keyfind("Location",2,Parsed),
    [?_assertMatch({1,required,"string","name",number,none},lists:keyfind(1,1,Person)),
     ?_assertMatch({2,required,"string","address",number,none},lists:keyfind(2,1,Person)),
     ?_assertMatch({3,required,"string","phone_number",number,none},lists:keyfind(3,1,Person)),
     ?_assertMatch({4,required,"int32","age",number,none},lists:keyfind(4,1,Person)),
     ?_assertMatch({5,repeated,"string","hobbies",number,none},lists:keyfind(5,1,Person)),
     ?_assertMatch({6,repeated,"Location","locations",number,none},lists:keyfind(6,1,Person)),
     ?_assertMatch({7,repeated_packed,"uint32","ids",number,none},lists:keyfind(7,1,Person)),
     ?_assertMatch({1,required,"string","region",number,none},lists:keyfind(1,1,Location)),
     ?_assertMatch({2,required,"string","country",number,none},lists:keyfind(2,1,Location))].

%%--------------------------------------------------------------------
%% Help functions
%%--------------------------------------------------------------------
parse(FileName) ->
    {ok, InFile} = file:open(FileName, [read]),
    Acc = loop(InFile,[]),
    file:close(InFile),
    {ok,Parsed} = protobuffs_parser:parse(Acc),
    Parsed.

loop(InFile,Acc) ->
    case io:request(InFile,{get_until,prompt,protobuffs_scanner,token,[1]}) of
        {ok,Token,_EndLine} ->
            loop(InFile,Acc ++ [Token]);
        {error,token} ->
            exit(scanning_error);    
        {eof,_} ->
            Acc
    end.

