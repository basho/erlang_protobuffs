-module(protobuffs_utils).

-export([
	collect_full_messages/1,
	function_exports/1,
	record_data/2,
	default_values/1,
	write_header/2
]).

write_header(Messages, Filename) ->
    {ok, FileRef} = file:open(Filename, [write]),
    lists:foreach(
        fun({Name, Fields}) ->
            OutFields = [string:to_lower(B) || {_A, _, _, B, _, _} <- lists:keysort(1, Fields)],
            JoinedFields = string:join(OutFields, ", "),
            io:format(FileRef, "-record(~s, {~s}).~n", [string:to_lower(Name), JoinedFields])
        end,
        Messages
    ),
    ok == file:close(FileRef).

collect_full_messages(Data) -> collect_full_messages(Data, []).

%% @hidden
collect_full_messages([], Acc) -> Acc;
collect_full_messages([{message, Name, Fields} | Tail], Acc) ->
    FieldsOut = lists:foldl(
        fun (Input, TmpAcc) ->
            case Input of
                {_, _, _, _, _, _} ->  [Input | TmpAcc];
                _ -> TmpAcc
            end
        end,
        [],
        Fields
    ),
    SubMessages = lists:foldl(
        fun ({message, C, D}, TmpAcc) -> [{message, C, D} | TmpAcc];
            (_, TmpAcc) -> TmpAcc
        end,
        [],
        Fields
    ),
    collect_full_messages(Tail ++ SubMessages, [{Name, FieldsOut} | Acc]).

function_exports(Messages) -> function_exports(Messages, []).

function_exports([{Name,_}|Tail], Acc) ->
	E = "encode_" ++ string:to_lower(Name) ++ "/1",
	D = "decode_" ++ string:to_lower(Name) ++ "/1", 
	function_exports(Tail, [E,D|Acc]);
function_exports([], Acc) -> string:join(Acc, ",").

record_data(Name, Fields) -> record_data(Name, lists:keysort(1, Fields), []).
	
record_data(Name, [{Position, Rule, FieldType, FieldName, _, Default}|Tail], Acc) ->
	Data = lists:flatten(io_lib:format("{~p, ~p, Rec#~s.~s, ~p, ~p}", [Position, Rule, string:to_lower(Name), FieldName, list_to_atom(string:to_lower(FieldType)), Default])),
	record_data(Name, Tail, [Data|Acc]);
record_data(_, [], Acc) -> string:join(lists:reverse(Acc), ",").

default_values(Fields) ->
	AllElements = lists:foldl(
	    fun(Field, Acc) ->
	        case Field of
	            {_, _, _, _, _, none} -> Acc;
	            {Position, _, _, _, _, Default} ->
	                [io_lib:format("{~p, ~p}", [Position, Default]) | Acc]
	        end
	    end,
	    [],
	    lists:keysort(1, Fields)
	),
	string:join(lists:reverse(AllElements), ", ").