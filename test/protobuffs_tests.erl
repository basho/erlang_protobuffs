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
    [?_assertMatch({1,optional,"double","real1",none},lists:keyfind(1,1,Messages)),
     ?_assertMatch({2,optional,"float","real2",none},lists:keyfind(2,1,Messages)),
     ?_assertMatch({3,optional,"int32","int1",none},lists:keyfind(3,1,Messages)),
     ?_assertMatch({4,optional,"int64","int2",none},lists:keyfind(4,1,Messages)),
     ?_assertMatch({5,optional,"uint32","int3",none},lists:keyfind(5,1,Messages)),
     ?_assertMatch({6,optional,"uint64","int4",none},lists:keyfind(6,1,Messages)),
     ?_assertMatch({7,optional,"sint32","int5",none},lists:keyfind(7,1,Messages)),
     ?_assertMatch({8,optional,"sint64","int6",none},lists:keyfind(8,1,Messages)),
     ?_assertMatch({9,optional,"fixed32","int7",none},lists:keyfind(9,1,Messages)),
     ?_assertMatch({10,optional,"fixed64","int8",none},lists:keyfind(10,1,Messages)),
     ?_assertMatch({11,optional,"sfixed32","int9",none},lists:keyfind(11,1,Messages)),
     ?_assertMatch({12,optional,"sfixed64","int10",none},lists:keyfind(12,1,Messages)),
     ?_assertMatch({13,optional,"bool","val1",none},lists:keyfind(13,1,Messages)),
     ?_assertMatch({14,optional,"string","str1",none},lists:keyfind(14,1,Messages)),
     ?_assertMatch({15,optional,"bytes","bit1",none},lists:keyfind(15,1,Messages))].

parse_has_default_test_() ->
    Path = filename:absname("../test/erlang_protobuffs_SUITE_data/hasdefault.proto"),
    [{message, "WithDefault", Messages}] = parse(Path),
    [?_assertMatch({1,required,"double","real1",1.0},lists:keyfind(1,1,Messages)),
     ?_assertMatch({2,required,"float","real2",2.0},lists:keyfind(2,1,Messages)),
     ?_assertMatch({3,required,"int32","int1",1},lists:keyfind(3,1,Messages)),
     ?_assertMatch({4,required,"int64","int2",2},lists:keyfind(4,1,Messages)),
     ?_assertMatch({5,required,"uint32","int3",3},lists:keyfind(5,1,Messages)),
     ?_assertMatch({6,required,"uint64","int4",4},lists:keyfind(6,1,Messages)),
     ?_assertMatch({7,required,"sint32","int5",5},lists:keyfind(7,1,Messages)),
     ?_assertMatch({8,required,"sint64","int6",6},lists:keyfind(8,1,Messages)),
     ?_assertMatch({9,required,"fixed32","int7",7},lists:keyfind(9,1,Messages)),
     ?_assertMatch({10,required,"fixed64","int8",8},lists:keyfind(10,1,Messages)),
     ?_assertMatch({11,required,"sfixed32","int9",9},lists:keyfind(11,1,Messages)),
     ?_assertMatch({12,required,"sfixed64","int10",10},lists:keyfind(12,1,Messages)),
     ?_assertMatch({13,required,"bool","val1",true},lists:keyfind(13,1,Messages)),
     ?_assertMatch({14,required,"string","str1","test"},lists:keyfind(14,1,Messages))].

parse_simple_test_() ->
    Path = filename:absname("../test/erlang_protobuffs_SUITE_data/simple.proto"),
    [{package,"simple"},
     {message, "Person", Person},
     {message, "Location", Location}] = parse(Path),
    [?_assertMatch({1,required,"string","name",none},lists:keyfind(1,1,Person)),
     ?_assertMatch({2,required,"string","address",none},lists:keyfind(2,1,Person)),
     ?_assertMatch({3,required,"string","phone_number",none},lists:keyfind(3,1,Person)),
     ?_assertMatch({4,required,"int32","age",none},lists:keyfind(4,1,Person)),
     ?_assertMatch({5,optional,"Location","location",none},lists:keyfind(5,1,Person)),
     ?_assertMatch({1,required,"string","region",none},lists:keyfind(1,1,Location)),
     ?_assertMatch({2,required,"string","country",none},lists:keyfind(2,1,Location))].

parse_enum_test_() ->
    Path = filename:absname("../test/erlang_protobuffs_SUITE_data/enum.proto"),
    [{message,"EnumMsg", EnumMsg}] = parse(Path),
    {enum, "Values", Values} = lists:keyfind(enum,1,EnumMsg),
    [?_assertMatch({1,optional,"Values","value",none},lists:keyfind(1,1,EnumMsg)),
     ?_assertMatch({'value1',1},lists:keyfind('value1',1,Values)),
     ?_assertMatch({'value2',2},lists:keyfind('value2',1,Values))].

parse_enum_outside_test_() ->
    Path = filename:absname("../test/erlang_protobuffs_SUITE_data/enum_outside.proto"),
    [{enum, "EnumList", Enums}, {message, "EnumUser", EnumUser}] = parse(Path),
    [?_assertMatch({1,optional,"EnumList", "enum_field",none},lists:keyfind(1,1,EnumUser)),
     ?_assertMatch({'FIRST',1},lists:keyfind('FIRST',1,Enums)),
     ?_assertMatch({'SECOND',2},lists:keyfind('SECOND',1,Enums))].

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
    [?_assertMatch({1,required,"string","name",none},lists:keyfind(1,1,Person)),
     ?_assertMatch({2,required,"int32","id",none},lists:keyfind(2,1,Person)),
     ?_assertMatch({3,optional,"string","email",none},lists:keyfind(3,1,Person)),
     ?_assertMatch({4,repeated,"PhoneNumber","phone",none},lists:keyfind(4,1,Person)),
     ?_assertMatch({1,required,"string","number",none},lists:keyfind(1,1,PhoneNumber)),
     ?_assertMatch({1,optional,"int32","mobile",none},lists:keyfind(1,1,PhoneType)),
     ?_assertMatch({2,optional,"int32","home",none},lists:keyfind(2,1,PhoneType)),
     ?_assertMatch({3,optional,"int32","work",none},lists:keyfind(3,1,PhoneType))].

parse_nested2_test_() ->
    Path = filename:absname("../test/erlang_protobuffs_SUITE_data/nested2.proto"),
    [{message, "Outer",Outer}] = parse(Path),
    {message, "MiddleAA", MiddleAA} = lists:keyfind("MiddleAA",2,Outer),
    {message, "MiddleBB", MiddleBB} = lists:keyfind("MiddleBB",2,Outer),
    {message, "Inner", InnerAA} = lists:keyfind("Inner",2,MiddleAA),
    {message, "Inner", InnerBB} = lists:keyfind("Inner",2,MiddleBB),
    [?_assertMatch({1,optional,"MiddleAA","middleaa",none},lists:keyfind(1,1,Outer)),
     ?_assertMatch({2,optional,"MiddleBB","middlebb",none},lists:keyfind(2,1,Outer)),
     ?_assertMatch({1,optional,"Inner","inner",none},lists:keyfind(1,1,MiddleAA)),
     ?_assertMatch({1,optional,"Inner","inner",none},lists:keyfind(1,1,MiddleBB)),
     ?_assertMatch({1,required,"int64","ival",none},lists:keyfind(1,1,InnerAA)),
     ?_assertMatch({2,optional,"bool","booly",none},lists:keyfind(2,1,InnerAA)),
     ?_assertMatch({1,required,"int32","ival",none},lists:keyfind(1,1,InnerBB)),
     ?_assertMatch({2,optional,"bool","booly",none},lists:keyfind(2,1,InnerBB))].

parse_nested3_test_() ->
    Path = filename:absname("../test/erlang_protobuffs_SUITE_data/nested3.proto"),
    [{message,"Outer",Outer}] = parse(Path),
    {message,"Middle",Middle} = lists:keyfind("Middle",2,Outer),
    {message,"Other",Other} = lists:keyfind("Other",2,Outer),
    {message,"Inner",Inner} = lists:keyfind("Inner",2,Middle),
    [?_assertMatch({1,optional,"Middle","middle",none},lists:keyfind(1,1,Outer)),
     ?_assertMatch({2,optional,"Inner","inner",none},lists:keyfind(2,1,Middle)),
     ?_assertMatch({3,optional,"Other","other",none},lists:keyfind(3,1,Middle)),
     ?_assertMatch({1,optional,"bool","foo",none},lists:keyfind(1,1,Inner)),
     ?_assertMatch({1,optional,"bool","bar",none},lists:keyfind(1,1,Other))].

parse_nested4_test_() ->
    Path = filename:absname("../test/erlang_protobuffs_SUITE_data/nested4.proto"),
    [{message,"Outer",Outer}] = parse(Path),
    {message,"Middle",Middle} = lists:keyfind("Middle",2,Outer),
    {message,"Other",Other} = lists:keyfind("Other",2,Outer),
    {message,"Inner",Inner} = lists:keyfind("Inner",2,Middle),
    [?_assertMatch({1,optional,"Middle","middle",none},lists:keyfind(1,1,Outer)),
     ?_assertMatch({2,optional,"Inner","inner",none},lists:keyfind(2,1,Middle)),
     ?_assertMatch({3,optional,"Other","other",none},lists:keyfind(3,1,Middle)),
     ?_assertMatch({1,optional,"bool","foo",none},lists:keyfind(1,1,Inner)),
     ?_assertMatch({1,optional,"bool","bar",none},lists:keyfind(1,1,Other))].

parse_nested5_test_() ->
    Path = filename:absname("../test/erlang_protobuffs_SUITE_data/nested5.proto"),
    Parsed = parse(Path),
    {message,"First",First} = lists:keyfind("First",2,Parsed),
    {message,"Second",Second} = lists:keyfind("Second",2,Parsed),
    {message,"Inner",Inner} = lists:keyfind("Inner",2,First),
    [?_assertMatch({1,optional,"Inner","inner",none},lists:keyfind(1,1,First)),
     ?_assertMatch({1,optional,"bool","foo",none},lists:keyfind(1,1,Inner)),
     ?_assertMatch({1,required,"First.Inner","inner",none},lists:keyfind(1,1,Second))].

parse_addressbook_test_() ->
    Path = filename:absname("../test/erlang_protobuffs_SUITE_data/addressbook.proto"),
    Parsed = parse(Path),
    {message,"Person",Person} = lists:keyfind("Person",2,Parsed),
    {message,"PhoneNumber",PhoneNumber} = lists:keyfind("PhoneNumber",2,Person),
    {enum, "PhoneType", PhoneType} = lists:keyfind(enum,1,Person),
    {message,"AddressBook",AddressBook} = lists:keyfind("AddressBook",2,Parsed),
    [?_assertMatch({1,required,"string","name",none},lists:keyfind(1,1,Person)),
     ?_assertMatch({2,required,"int32","id",none},lists:keyfind(2,1,Person)),
     ?_assertMatch({3,optional,"string","email",none},lists:keyfind(3,1,Person)),
     ?_assertMatch({4,repeated,"PhoneNumber","phone",none},lists:keyfind(4,1,Person)),
     ?_assertMatch({1,required,"string","number",none},lists:keyfind(1,1,PhoneNumber)),
     ?_assertMatch({2,optional,"PhoneType","type",'HOME'},lists:keyfind(2,1,PhoneNumber)),
     ?_assertMatch({1,repeated,"Person","person",none},lists:keyfind(1,1,AddressBook)),
     ?_assertMatch({'MOBILE',0},lists:keyfind('MOBILE',1,PhoneType)),
     ?_assertMatch({'HOME',1},lists:keyfind('HOME',1,PhoneType)),
     ?_assertMatch({'WORK',2},lists:keyfind('WORK',1,PhoneType))].

parse_repeater_test_() ->
    Path = filename:absname("../test/erlang_protobuffs_SUITE_data/repeater.proto"),
    Parsed = parse(Path),
    {message,"Person",Person} = lists:keyfind("Person",2,Parsed),
    {message,"Location",Location} = lists:keyfind("Location",2,Parsed),
    [?_assertMatch({1,required,"string","name",none},lists:keyfind(1,1,Person)),
     ?_assertMatch({2,required,"string","address",none},lists:keyfind(2,1,Person)),
     ?_assertMatch({3,required,"string","phone_number",none},lists:keyfind(3,1,Person)),
     ?_assertMatch({4,required,"int32","age",none},lists:keyfind(4,1,Person)),
     ?_assertMatch({5,repeated,"string","hobbies",none},lists:keyfind(5,1,Person)),
     ?_assertMatch({6,repeated,"Location","locations",none},lists:keyfind(6,1,Person)),
     ?_assertMatch({7,repeated,"uint32","ids",none},lists:keyfind(7,1,Person)),
     ?_assertMatch({1,required,"string","region",none},lists:keyfind(1,1,Location)),
     ?_assertMatch({2,required,"string","country",none},lists:keyfind(2,1,Location))].

parse_packed_repeated_test_() ->
    Path = filename:absname("../test/erlang_protobuffs_SUITE_data/packed_repeated.proto"),
    Parsed = parse(Path),
    {message,"Person",Person} = lists:keyfind("Person",2,Parsed),
    {message,"Location",Location} = lists:keyfind("Location",2,Parsed),
    [?_assertMatch({1,required,"string","name",none},lists:keyfind(1,1,Person)),
     ?_assertMatch({2,required,"string","address",none},lists:keyfind(2,1,Person)),
     ?_assertMatch({3,required,"string","phone_number",none},lists:keyfind(3,1,Person)),
     ?_assertMatch({4,required,"int32","age",none},lists:keyfind(4,1,Person)),
     ?_assertMatch({5,repeated,"string","hobbies",none},lists:keyfind(5,1,Person)),
     ?_assertMatch({6,repeated,"Location","locations",none},lists:keyfind(6,1,Person)),
     ?_assertMatch({7,repeated_packed,"uint32","ids",none},lists:keyfind(7,1,Person)),
     ?_assertMatch({1,required,"string","region",none},lists:keyfind(1,1,Location)),
     ?_assertMatch({2,required,"string","country",none},lists:keyfind(2,1,Location))].

parse_imported_test_() ->
    Path = filename:absname("../test/erlang_protobuffs_SUITE_data/import.proto"),
    Parsed = parse(Path),
    [?_assertEqual(false, lists:keyfind("Imported", 2, Parsed)),
     ?_assertMatch({message, "Foo", _Foo}, lists:keyfind("Foo", 2, Parsed))].

parse_extend_out_of_range_test_() ->
    DataDir = "../test/erlang_protobuffs_SUITE_data",
    Path = filename:absname(filename:join([DataDir,"extend_out_of_range.proto"])),
    Error = (catch protobuffs_compile:scan_file(Path, [{imports_dir, [DataDir]}])),
    [?_assertEqual(out_of_range, Error)].

parse_extend_in_reserved_range_test_() ->
    DataDir = "../test/erlang_protobuffs_SUITE_data",
    Path = filename:absname(filename:join([DataDir,"extend_in_reserved_range.proto"])),
    Error = (catch protobuffs_compile:scan_file(Path, [{imports_dir, [DataDir]}])),
    [?_assertEqual(out_of_range, Error)].

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

