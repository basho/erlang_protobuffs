%%%-------------------------------------------------------------------
%%% @author David Åberg <>
%%% @copyright (C) 2010, David Åberg
%%% @doc
%%%
%%% @end
%%% Created :  4 Nov 2010 by David Åberg <>
%%%-------------------------------------------------------------------
-module(erlang_protobuffs_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    [{num_tests,1000}|Config].

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%% @end
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() -> 
    [protobuffs_test_case,
     protobuffs_packed_test_case,
     parse_empty_test_case, 
     parse_has_default_test_case,
     parse_simple_test_case,
     parse_nested1_test_case,
     parse_nested2_test_case,
     parse_nested3_test_case,
     parse_nested4_test_case,
     parse_nested5_test_case,
     parse_enum_test_case,
     parse_enum_outside_test_case,
     parse_extensions_test_case,
     parse_addressbook_test_case,
     parse_repeater_test_case,
     parse_packed_repeated_test_case,
     parse_special_words_test_case].

%%--------------------------------------------------------------------
%% @spec TestCase() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
protobuffs_test_case() ->
    [].
protobuffs_packed_test_case() ->
    [].
parse_empty_test_case() -> 
    [].
parse_has_default_test_case() ->
    [].
parse_simple_test_case() ->
    [].
parse_nested1_test_case() ->
    [].
parse_nested2_test_case() ->
    [].
parse_nested3_test_case() ->
    [].
parse_nested4_test_case() ->
    [].
parse_nested5_test_case() ->
    [].
parse_enum_test_case() ->
    [].
parse_enum_outside_test_case() ->
	[].
parse_extensions_test_case() ->
	[].
parse_addressbook_test_case() ->
    [].
parse_repeater_test_case() ->
    [].
parse_packed_repeated_test_case() ->
    [].
parse_special_words_test_case() ->
    [].

%%--------------------------------------------------------------------
%% @spec TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%% @end
%%--------------------------------------------------------------------
protobuffs_test_case(Config) ->
    NumTests = ?config(num_tests, Config),
    true = eqc:quickcheck(eqc:numtests(NumTests,protobuffs_eqc:prop_protobuffs())).

protobuffs_packed_test_case(Config) ->
    NumTests = ?config(num_tests, Config),
    true = eqc:quickcheck(eqc:numtests(NumTests,protobuffs_eqc:prop_protobuffs_packed())).

parse_empty_test_case(Config) -> 
    DataDir = ?config(data_dir, Config),
    NumTests = ?config(num_tests, Config),
    Path = filename:absname(filename:join([DataDir,"empty.proto"])),
    test_server:format("Testcase ~p, parse file ~p~n", [self(), Path]),
    protobuffs_compile:scan_file(Path),
    true = eqc:quickcheck(eqc:numtests(NumTests,protobuffs_eqc:prop_protobuffs_empty())).

parse_has_default_test_case(Config) ->
    DataDir = ?config(data_dir, Config),
    NumTests = ?config(num_tests, Config),
    Path = filename:absname(filename:join([DataDir,"hasdefault.proto"])),
    protobuffs_compile:scan_file(Path),
    true = eqc:quickcheck(eqc:numtests(NumTests,protobuffs_eqc:prop_protobuffs_has_default())).

parse_simple_test_case(Config) ->
    DataDir = ?config(data_dir, Config),
    NumTests = ?config(num_tests, Config),
    Path = filename:absname(filename:join([DataDir,"simple.proto"])),
    protobuffs_compile:scan_file(Path),
    true = eqc:quickcheck(eqc:numtests(NumTests,protobuffs_eqc:prop_protobuffs_simple())).

parse_nested1_test_case(Config) ->
    DataDir = ?config(data_dir, Config),
    NumTests = ?config(num_tests, Config),
    Path = filename:absname(filename:join([DataDir,"nested1.proto"])),
    protobuffs_compile:scan_file(Path),
    true = eqc:quickcheck(eqc:numtests(NumTests,protobuffs_eqc:prop_protobuffs_nested1())).

parse_nested2_test_case(Config) ->
    DataDir = ?config(data_dir, Config),
    NumTests = ?config(num_tests, Config),
    Path = filename:absname(filename:join([DataDir,"nested2.proto"])),
    protobuffs_compile:scan_file(Path),
    true = eqc:quickcheck(eqc:numtests(NumTests,protobuffs_eqc:prop_protobuffs_nested2())).

parse_nested3_test_case(Config) ->
    DataDir = ?config(data_dir, Config),
    NumTests = ?config(num_tests, Config),
    Path = filename:absname(filename:join([DataDir,"nested3.proto"])),
    protobuffs_compile:scan_file(Path),
    true = eqc:quickcheck(eqc:numtests(NumTests,protobuffs_eqc:prop_protobuffs_nested3())).

parse_nested4_test_case(Config) ->
    DataDir = ?config(data_dir, Config),
    NumTests = ?config(num_tests, Config),
    Path = filename:absname(filename:join([DataDir,"nested4.proto"])),
    protobuffs_compile:scan_file(Path),
    true = eqc:quickcheck(eqc:numtests(NumTests,protobuffs_eqc:prop_protobuffs_nested4())).

parse_nested5_test_case(Config) ->
    DataDir = ?config(data_dir, Config),
    NumTests = ?config(num_tests, Config),
    Path = filename:absname(filename:join([DataDir,"nested5.proto"])),
    protobuffs_compile:scan_file(Path),
    true = eqc:quickcheck(eqc:numtests(NumTests,protobuffs_eqc:prop_protobuffs_nested5_1())),
    true = eqc:quickcheck(eqc:numtests(NumTests,protobuffs_eqc:prop_protobuffs_nested5_2())).

parse_enum_test_case(Config) ->
    DataDir = ?config(data_dir, Config),
    NumTests = ?config(num_tests, Config),
    Path = filename:absname(filename:join([DataDir,"enum.proto"])),
    protobuffs_compile:scan_file(Path),
    true = eqc:quickcheck(eqc:numtests(NumTests,protobuffs_eqc:prop_protobuffs_enum())).

parse_enum_outside_test_case(Config) ->
	DataDir = ?config(data_dir, Config),
	NumTests = ?config(num_tests, Config),
	Path = filename:absname(filename:join([DataDir, "enum_outside.proto"])),
	protobuffs_compile:scan_file(Path),
	true = eqc:quickcheck(eqc:numtests(NumTests,protobuffs_eqc:prop_protobuffs_enum_outside())).

parse_extensions_test_case(Config) ->
	DataDir = ?config(data_dir, Config),
	NumTests = ?config(num_tests, Config),
	Path = filename:absname(filename:join([DataDir, "extensions.proto"])),
	protobuffs_compile:scan_file(Path),
	true = eqc:quickcheck(eqc:numtests(NumTests,protobuffs_eqc:prop_protobuffs_extentions())).

parse_service_test_case(Config) ->
	DataDir = ?config(data_dir, Config),
	NumTests = ?config(num_tests, Config),
	Path = filename:absname(filename:join([DataDir, "service.proto"])),
	protobuffs_compile:scan_file(Path),
	true = eqc:quickcheck(eqc:numtests(NumTests,protobuffs_eqc:prop_protobuffs_extentions())).

parse_addressbook_test_case(Config) ->
    DataDir = ?config(data_dir, Config),
    NumTests = ?config(num_tests, Config),
    Path = filename:absname(filename:join([DataDir,"addressbook.proto"])),
    protobuffs_compile:scan_file(Path),
    true = eqc:quickcheck(eqc:numtests(NumTests,protobuffs_eqc:prop_protobuffs_addressbook())).

parse_repeater_test_case(Config) ->
    DataDir = ?config(data_dir, Config),
    NumTests = ?config(num_tests, Config),
    Path = filename:absname(filename:join([DataDir,"repeater.proto"])),
    protobuffs_compile:scan_file(Path),
    true = eqc:quickcheck(eqc:numtests(NumTests,protobuffs_eqc:prop_protobuffs_repeater())).

parse_packed_repeated_test_case(Config) ->
    DataDir = ?config(data_dir, Config),
    NumTests = ?config(num_tests, Config),
    Path = filename:absname(filename:join([DataDir,"packed_repeated.proto"])),
    protobuffs_compile:scan_file(Path),
    true = eqc:quickcheck(eqc:numtests(NumTests,protobuffs_eqc:prop_protobuffs_packed_repeated())).

parse_special_words_test_case(Config) ->
    DataDir = ?config(data_dir, Config),
    NumTests = ?config(num_tests, Config),
    Path = filename:absname(filename:join([DataDir,"special_words.proto"])),
    protobuffs_compile:scan_file(Path),
    true = eqc:quickcheck(eqc:numtests(NumTests,protobuffs_eqc:prop_protobuffs_special_words())).



%%---------------------------------------------------------------------
%% Help flies
%%---------------------------------------------------------------------
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
