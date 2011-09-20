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
suite() -> [{timetrap, {seconds, 60}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) -> [{num_tests, 1000} | Config].

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) -> ok.

%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) -> Config.

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) -> ok.

%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) -> Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) -> ok.

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
groups() -> [].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() ->
    [protobuffs_test_case, protobuffs_packed_test_case, test_proto_files].

%%--------------------------------------------------------------------
%% @spec TestCase() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
protobuffs_test_case() -> [].

protobuffs_packed_test_case() -> [].

test_proto_files() -> [].

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
    NumTests = (?config(num_tests, Config)),
    true = proper:quickcheck(proper:numtests(NumTests,
					     protobuffs_proper:proper_protobuffs())).

protobuffs_packed_test_case(Config) ->
    NumTests = (?config(num_tests, Config)),
    true = proper:quickcheck(proper:numtests(NumTests,
					     protobuffs_proper:proper_protobuffs_packed())).

test_proto_files(Config) ->
    DataDir = (?config(data_dir, Config)),
    NumTests = (?config(num_tests, Config)),
    ProtoFiles = filelib:wildcard(filename:join([DataDir,"proto","*.proto"])),
    
    TestProtoFile = fun(ProtoFile,Acc) ->
			    Path = filename:absname(ProtoFile),
			    Message = filename:basename(ProtoFile,".proto"),
			    test_server:format("Testcase ~p, parse file ~p~n",
					       [self(), Path]),
			    protobuffs_compile:scan_file(
			      Path,
			      [{imports_dir, [filename:join([DataDir,"proto"]),
					      filename:join([DataDir,"proto","import"])]}]),
			    test_server:format("Testcase ~p, testing message ~p~n",
					       [self(), Message]),
			    Test = list_to_atom("proper_protobuffs_"++Message),
			    Acc andalso proper:quickcheck(
					  proper:numtests(
					    NumTests,
					    protobuffs_proper:Test()),[quiet])
		    end,
    lists:foldl(TestProtoFile,true,ProtoFiles).

%%---------------------------------------------------------------------
%% Help flies
%%---------------------------------------------------------------------
parse(FileName) ->
    {ok, InFile} = file:open(FileName, [read]),
    Acc = loop(InFile, []),
    file:close(InFile),
    {ok, Parsed} = protobuffs_parser:parse(Acc),
    Parsed.

loop(InFile, Acc) ->
    case io:request(InFile,
		    {get_until, prompt, protobuffs_scanner, token, [1]})
    of
	{ok, Token, _EndLine} -> loop(InFile, Acc ++ [Token]);
	{error, token} -> exit(scanning_error);
	{eof, _} -> Acc
    end.
