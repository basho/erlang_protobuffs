%%%-------------------------------------------------------------------
%%% File    : protobuffs_tests.erl
%%% Author  : David AAberg <david_ab@RB-DAVIDAB01>
%%% Description : 
%%%
%%% Created :  2 Aug 2010 by David AAberg <david_ab@RB-DAVIDAB01>
%%%-------------------------------------------------------------------
-module(protobuffs_tests).

-include_lib("eunit/include/eunit.hrl").

-define(NUM_TESTS,1000).

parse_empty_file_test_() ->
    Path = filename:absname("tests/empty.proto"),
    [{message, "Empty", Messages}] = protobuffs_parser:parse_file(Path),
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
    Path = filename:absname("tests/hasdefault.proto"),
    [{message, "WithDefault", Messages}] = protobuffs_parser:parse_file(Path),
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
    Path = filename:absname("tests/simple.proto"),
    [{package,"simple"},
     {message, "Person", Person},
     {message, "Location", Location}] = protobuffs_parser:parse_file(Path),
    [?_assertMatch({1,required,"string","name",number,none},lists:keyfind(1,1,Person)),
     ?_assertMatch({2,required,"string","address",number,none},lists:keyfind(2,1,Person)),
     ?_assertMatch({3,required,"string","phone_number",number,none},lists:keyfind(3,1,Person)),
     ?_assertMatch({4,required,"int32","age",number,none},lists:keyfind(4,1,Person)),
     ?_assertMatch({5,optional,"Location","location",number,none},lists:keyfind(5,1,Person)),
     ?_assertMatch({1,required,"string","region",number,none},lists:keyfind(1,1,Location)),
     ?_assertMatch({2,required,"string","country",number,none},lists:keyfind(2,1,Location))].

parse_enum_test_() ->
    Path = filename:absname("tests/enum.proto"),
    [{message,"EnumMsg", EnumMsg}] = protobuffs_parser:parse_file(Path),
    {enum, "Values", Values} = lists:keyfind(enum,1,EnumMsg),
    [?_assertMatch({1,optional,"Values","value",number,none},lists:keyfind(1,1,EnumMsg)),
     ?_assertMatch({enum,1,"value1"},lists:keyfind("value1",3,Values)),
     ?_assertMatch({enum,2,"value2"},lists:keyfind("value2",3,Values))].

parse_nested1_test_() ->
    Path = filename:absname("tests/nested1.proto"),
    [{message, "Person", Person}] = protobuffs_parser:parse_file(Path),
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
    Path = filename:absname("tests/nested2.proto"),
    [{message, "Outer",Outer}] = protobuffs_parser:parse_file(Path),
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
    Path = filename:absname("tests/nested3.proto"),
    [{message,"Outer",Outer}] = protobuffs_parser:parse_file(Path),
    {message,"Middle",Middle} = lists:keyfind("Middle",2,Outer),
    {message,"Other",Other} = lists:keyfind("Other",2,Outer),
    {message,"Inner",Inner} = lists:keyfind("Inner",2,Middle),
    [?_assertMatch({1,optional,"Middle","middle",number,none},lists:keyfind(1,1,Outer)),
     ?_assertMatch({2,optional,"Inner","inner",number,none},lists:keyfind(2,1,Middle)),
     ?_assertMatch({3,optional,"Other","other",number,none},lists:keyfind(3,1,Middle)),
     ?_assertMatch({1,optional,"bool","foo",number,none},lists:keyfind(1,1,Inner)),
     ?_assertMatch({1,optional,"bool","bar",number,none},lists:keyfind(1,1,Other))].

parse_nested4_test_() ->
    Path = filename:absname("tests/nested4.proto"),
    [{message,"Outer",Outer}] = protobuffs_parser:parse_file(Path),
    {message,"Middle",Middle} = lists:keyfind("Middle",2,Outer),
    {message,"Other",Other} = lists:keyfind("Other",2,Outer),
    {message,"Inner",Inner} = lists:keyfind("Inner",2,Middle),
    [?_assertMatch({1,optional,"Middle","middle",number,none},lists:keyfind(1,1,Outer)),
     ?_assertMatch({2,optional,"Inner","inner",number,none},lists:keyfind(2,1,Middle)),
     ?_assertMatch({3,optional,"Other","other",number,none},lists:keyfind(3,1,Middle)),
     ?_assertMatch({1,optional,"bool","foo",number,none},lists:keyfind(1,1,Inner)),
     ?_assertMatch({1,optional,"bool","bar",number,none},lists:keyfind(1,1,Other))].

parse_nested5_test_() ->
    Path = filename:absname("tests/nested5.proto"),
    Parsed = protobuffs_parser:parse_file(Path),
    {message,"First",First} = lists:keyfind("First",2,Parsed),
    {message,"Second",Second} = lists:keyfind("Second",2,Parsed),
    {message,"Inner",Inner} = lists:keyfind("Inner",2,First),
    [?_assertMatch({1,optional,"Inner","inner",number,none},lists:keyfind(1,1,First)),
     ?_assertMatch({1,optional,"bool","foo",number,none},lists:keyfind(1,1,Inner)),
     ?_assertMatch({1,required,"First.Inner","inner",number,none},lists:keyfind(1,1,Second))].

prop_protobuffs_test_() ->
    [?_assert(eqc:quickcheck(eqc:numtests(?NUM_TESTS,protobuffs_eqc:prop_protobuffs())))].

empty_setup() ->
    Path = filename:absname("tests/empty.proto"),
    protobuffs_compile:scan_file(Path).

teardown(_) ->
    ok.

protobuffs_empty_test_() ->
    {
      setup, 
      fun empty_setup/0, 
      fun teardown/1,
      fun(_) ->
	      [?_assert(
		  eqc:quickcheck(
		    eqc:numtests(?NUM_TESTS,protobuffs_eqc:prop_protobuffs_empty())
		   )
		 )
	      ]
      end
    }.

has_default_setup() ->
    Path = filename:absname("tests/hasdefault.proto"),
    protobuffs_compile:scan_file(Path).

protobuffs_has_default_test_() ->
    {
      setup, 
      fun has_default_setup/0, 
      fun teardown/1,
      fun(_) ->
	      [?_assert(
		  eqc:quickcheck(
		    eqc:numtests(?NUM_TESTS,protobuffs_eqc:prop_protobuffs_has_default())
		   )
		 )
	      ]
      end
    }.

simple_setup() ->
    Path = filename:absname("tests/simple.proto"),
    protobuffs_compile:scan_file(Path).

protobuffs_simple_test_() ->
    {
      setup, 
      fun simple_setup/0, 
      fun teardown/1,
      fun(_) ->
	      [?_assert(
		  eqc:quickcheck(
		    eqc:numtests(?NUM_TESTS,protobuffs_eqc:prop_protobuffs_simple())
		   )
		 )
	      ]
      end
    }.

nested1_setup() ->
    Path = filename:absname("tests/nested1.proto"),
    protobuffs_compile:scan_file(Path).

protobuffs_nested1_test_() ->
    {
      setup, 
      fun nested1_setup/0, 
      fun teardown/1,
      fun(_) ->
	      [?_assert(
		  eqc:quickcheck(
		    eqc:numtests(?NUM_TESTS,protobuffs_eqc:prop_protobuffs_nested1())
		   )
		 )
	      ]
      end
    }.

nested2_setup() ->
    Path = filename:absname("tests/nested2.proto"),
    protobuffs_compile:scan_file(Path).

protobuffs_nested2_test_() ->
    {
      setup, 
      fun nested2_setup/0, 
      fun teardown/1,
      fun(_) ->
	      [?_assert(
		  eqc:quickcheck(
		    eqc:numtests(?NUM_TESTS,protobuffs_eqc:prop_protobuffs_nested2())
		   )
		 )
	      ]
      end
    }.

nested3_setup() ->
    Path = filename:absname("tests/nested3.proto"),
    protobuffs_compile:scan_file(Path).

protobuffs_nested3_test_() ->
    {
      setup, 
      fun nested3_setup/0, 
      fun teardown/1,
      fun(_) ->
	      [?_assert(
		  eqc:quickcheck(
		    eqc:numtests(?NUM_TESTS,protobuffs_eqc:prop_protobuffs_nested3())
		   )
		 )
	      ]
      end
    }.

nested4_setup() ->
    Path = filename:absname("tests/nested4.proto"),
    protobuffs_compile:scan_file(Path).

protobuffs_nested4_test_() ->
    {
      setup, 
      fun nested4_setup/0, 
      fun teardown/1,
      fun(_) ->
	      [?_assert(
		  eqc:quickcheck(
		    eqc:numtests(?NUM_TESTS,protobuffs_eqc:prop_protobuffs_nested4())
		   )
		 )
	      ]
      end
    }.

nested5_setup() ->
    Path = filename:absname("tests/nested5.proto"),
    protobuffs_compile:scan_file(Path).

protobuffs_nested5_test_() ->
    {
      setup, 
      fun nested5_setup/0, 
      fun teardown/1,
      fun(_) ->
	      [?_assert(
		  eqc:quickcheck(
		    eqc:numtests(?NUM_TESTS,protobuffs_eqc:prop_protobuffs_nested5_1())
		   )
		 ),
	       ?_assert(
		  eqc:quickcheck(
		    eqc:numtests(?NUM_TESTS,protobuffs_eqc:prop_protobuffs_nested5_2())
		   )
		 )
	      ]
      end
    }.

enum_setup() ->
    Path = filename:absname("tests/enum.proto"),
    protobuffs_compile:scan_file(Path).

protobuffs_enum_test_() ->
    {
      setup, 
      fun enum_setup/0, 
      fun teardown/1,
      fun(_) ->
	      [?_assert(
		  eqc:quickcheck(
		    eqc:numtests(?NUM_TESTS,protobuffs_eqc:prop_protobuffs_enum())
		   )
		 )
	      ]
      end
    }.
