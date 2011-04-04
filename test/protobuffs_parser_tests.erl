%%%-------------------------------------------------------------------
%%% @author David Åberg <davabe@hotmail.com>
%%% @copyright (C) 2011, David Åberg
%%% @doc
%%%
%%% @end
%%% Created :  5 Feb 2011 by David Åberg <davabe@hotmail.com>
%%%-------------------------------------------------------------------
-module(protobuffs_parser_tests).

-include_lib("eunit/include/eunit.hrl").

package_test_() ->
    {ok,Result,1} = protobuffs_scanner:string("package \"test.package\";"),
    Parsed = protobuffs_parser:parse(Result),
    [?_assertMatch({ok,[{package,"test.package"}]},Parsed)].

import_test_() ->
    {ok,Result,1} = protobuffs_scanner:string("import \"test.package\";"),
    Parsed = protobuffs_parser:parse(Result),
    [?_assertMatch({ok,[{import,"test.package"}]},Parsed)].

message_test_() ->
    {ok,Result,1} = protobuffs_scanner:string("message Test { required string name = 1; }"),
    Parsed = protobuffs_parser:parse(Result),
    [?_assertMatch({ok,[{message, "Test",[{1,required,"string","name",none}]}]},Parsed)].

message_default_test_() ->
    {ok,Result,1} = protobuffs_scanner:string("message Test { optional float value = 1 [default=0.01]; }"),
    Parsed = protobuffs_parser:parse(Result),
    [?_assertMatch({ok,[{message, "Test",[{1,optional,"float","value",0.01}]}]},Parsed)].

packed_test_() ->
    {ok,Result,1} = protobuffs_scanner:string("message Test { repeated float values = 1 [packed=true]; }"),
    Parsed = protobuffs_parser:parse(Result),
    [?_assertMatch({ok,[{message, "Test",[{1,repeated_packed,"float","values",none}]}]},Parsed)].

enum_test_() ->
    {ok,Result,1} = protobuffs_scanner:string("enum MyEnum { VALUE0 = 0; VALUE1 = 1;}"),
    Parsed = protobuffs_parser:parse(Result),
    [?_assertMatch({ok,[{enum, "MyEnum", [{'VALUE0',0},{'VALUE1',1}]}]},Parsed)].

service_test_() ->
    {ok,Result,1} = protobuffs_scanner:string("service SearchService { rpc Search (SearchRequest) returns (SearchResponse);}"),
    Parsed = protobuffs_parser:parse(Result),
    [?_assertMatch({ok,[{service, "SearchService", [{rpc, "Search", "SearchRequest", "SearchResponse"}]}]},Parsed)].

extensions_test_() ->
    {ok,Result,1} = protobuffs_scanner:string("message Foo { extensions 100 to 199; }"),
    Parsed = protobuffs_parser:parse(Result),
    [?_assertMatch({ok,[{message, "Foo", [{extensions, 100, 199}]}]},Parsed)].

extend_test_() ->
    {ok, Result, 1} = protobuffs_scanner:string("extend Foo { optional int32 bar = 126; }"),
    Parsed = protobuffs_parser:parse(Result),
    [?_assertMatch({ok,[{extend, "Foo", [{126, optional, "int32", "bar", none}]}]}, Parsed)].

option_test_() ->
    {ok,Result,1} = protobuffs_scanner:string("option message_set_wire_format = true;"),
    Parsed = protobuffs_parser:parse(Result),
    [?_assertMatch({ok,[{option,message_set_wire_format,true}]}, Parsed)].

inner_option_test_() ->
    {ok,Result,1} = protobuffs_scanner:string("message Foo { option message_set_wire_format = true;}"),
    Parsed = protobuffs_parser:parse(Result),
    [?_assertMatch({ok,[{message, "Foo", [{option,message_set_wire_format,true}]}]}, Parsed)].

