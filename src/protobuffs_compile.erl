%% @doc Create modules for decoding and encoding protocolo buffers messages out of .proto files.
-module(protobuffs_compile).
-export([scan_file/1]).

%% @spec scan_file(string()) -> ok
%% @doc Scan a .proto file and try to create a module for it. This process
%% creates a number of encoding, decoding and validation functions for each
%% message contained.
scan_file(Filename) ->
    {ok, Data} = file:read_file(Filename),
    Raw = scan(binary_to_list(Data)),
    Parsed = parse(Raw, []),
    true = write_header(Parsed, filename:basename(Filename, ".proto") ++ "_pb.hrl"),
    true = write_module(Parsed, filename:basename(Filename, ".proto") ++ "_pb.erl"),
    ok.

%% @hidden
parse(Data) -> parse(Data, []).

%% @hidden
parse([], Acc) -> lists:reverse(Acc);
parse([{'}', _Line} | Tail], Acc) -> {Acc, Tail};
parse([{enum, _Line}, {bareword, _Line, MessageName}, {'{', _Line} | Tail], Acc) ->
    {Res, Tail2} = parse(Tail, []),
    parse(Tail2, [{enum, MessageName, lists:reverse(Res)} | Acc]);
parse([{message, _Line}, {bareword, _Line, MessageName}, {'{', _Line} | Tail], Acc) ->
    {Res, Tail2} = parse(Tail, []),
    parse(Tail2, [{message, MessageName, lists:reverse(Res)} | Acc]);
parse([{bareword, _Line, FieldName}, {'=', _Line}, {number, _Line, Value}, {';', _Line} | Tail], Acc) ->
    parse(Tail, [{enum, Value, FieldName} | Acc]);
parse([{Type, _Line}, {bareword, _Line, Field}, {bareword, _Line, FieldName}, {'=', _Line}, {FieldType, _Line, Position}, {'[', _Line}, {bareword, _Line,"default"}, {'=', _Line}, {bareword, _Line, Default}, {']', _Line}, {';', _Line} | Tail], Acc) ->
    parse(Tail, [{Position, Type, Field, FieldName, FieldType, Default} | Acc]);
parse([{Type, _Line}, {bareword, _Line, Field}, {bareword, _Line, FieldName}, {'=', _Line}, {FieldType, _Line, Position}, {';', _Line} | Tail], Acc) ->
    parse(Tail, [{Position, Type, Field, FieldName, FieldType, none} | Acc]);
parse([{'$end', _} | Tail], Acc) ->
    parse(Tail, Acc);
parse([Head | Tail], Acc) ->
    parse(Tail, [Head | Acc]).

%% @hidden
write_header(Data, Filename) ->
    Messages = collect_messages(Data, []),
    {ok, FileRef} = file:open(Filename, [write]),
    lists:foreach(
        fun({Name, Fields}) ->
            OutFields = [string:to_lower(B) || {_A, B} <- lists:keysort(1, Fields)],
            JoinedFields = string:join(OutFields, ", "),
            io:format(FileRef, "-record(~s, {~s}).~n", [string:to_lower(Name), JoinedFields])
        end,
        Messages
    ),
    ok == file:close(FileRef).

%% @hidden
write_module(Data, Filename) ->
    Messages = collect_messages(Data, []),
    {ok, FileRef} = file:open(Filename, [write]),
    io:format(FileRef, "-module(~s).~n", [filename:basename(Filename, ".erl")]),
    DecodeString = string:join(
        ["decode_" ++ string:to_lower(Name) ++ "/1" || {Name, _} <- Messages] ++
        ["encode_" ++ string:to_lower(Name) ++ "/1" || {Name, _} <- Messages]
        ,", "),
    io:format(FileRef, "-export([~s]).~n", [DecodeString]),
    io:format(FileRef, "-include(\"~s\").~n~n", [filename:basename(Filename, ".erl") ++ ".hrl"]),
    write_decode_message(FileRef, Messages),
    write_encode_message(FileRef, collect_full_messages(Data, [])),
    ok == file:close(FileRef).

%% @hidden
write_encode_message(_, []) -> ok;
write_encode_message(FileRef, [{Name, Fields} |Tail]) ->
    EncodeElements = lists:foldl(
        fun(Field, Acc) ->
            {Position, _, FieldType, FieldName, _, _} = Field,
            [io_lib:format("{~p, Rec#~s.~s, ~p}", [Position, string:to_lower(Name), FieldName, list_to_atom(string:to_lower(FieldType))]) | Acc]
        end,
        [],
        lists:keysort(1, Fields)
    ),
    EncodeString = string:join(lists:reverse(EncodeElements), ", "),
    io:format(
        FileRef,
        "encode_~s(Rec) -> ~n"
        "   EncodeData = [~s], ~n"
        "   erlang:iolist_to_binary(lists:reverse(lists:foldl(fun({Pos, Data, Type}, Acc) -> 
                case [Data, Type] of 
                    [undefined, _] -> Acc; 
                    [Data, Type] when is_binary(Data), Type =/= bytes ->
                        [protobuffs:encode(Pos, Data, bytes) | Acc];
                    [Data, Type] when is_tuple(Data) ->
                        [RecName | _] = erlang:tuple_to_list(Data),
                        ToEncode = apply(?MODULE, list_to_atom(\"encode_\" ++ atom_to_list(RecName)), [Data]),
                        [protobuffs:encode(Pos, ToEncode, bytes) | Acc];
                    _ -> [protobuffs:encode(Pos, Data, Type) | Acc] 
                end 
            end,[], EncodeData))). ~n~n",
        [string:to_lower(Name), EncodeString]
    ),
    write_encode_message(FileRef, Tail).

%% @hidden
write_decode_message(_, []) -> ok;
write_decode_message(FileRef, [{Name, Fields} | Tail]) ->
    io:format(
        FileRef,
        "decode_~s(Data) when is_binary(Data) -> ~n"
        "   DecodedData = protobuffs:decode_many(Data),~n"
        "   ~s_to_record(DecodedData).~n~n",
        [string:to_lower(Name), string:to_lower(Name)]
    ),
    CasePosString = lists:foldl(
        fun({FPos, FName}, Acc) ->
            io_lib:format("     {~p, Data} -> Rec#~s{ ~s = Data};~n", [FPos, string:to_lower(Name), FName]) ++ Acc
        end,
        "",
        Fields
    ),
    io:format(
        FileRef,
        "~s_to_record(DecodedData) -> ~n"
        "   ~s_to_record(DecodedData, #~s{}). ~n"
        "~s_to_record([], Acc) -> Acc; ~n"
        "~s_to_record([Head | Tail], Rec) -> ~n"
        "   NewRec = case Head of ~n~s"
        "       _ -> Rec %% Ruh-roh ~n"
        "   end, ~n"
        "   ~s_to_record(Tail, NewRec).~n~n",
        [
            string:to_lower(Name), string:to_lower(Name),
            string:to_lower(Name), string:to_lower(Name),
            string:to_lower(Name), 
            CasePosString, string:to_lower(Name)
        ]
    ),
    write_decode_message(FileRef, Tail).

%% @hidden
collect_messages([], Acc) -> Acc;
collect_messages([{message, Name, Fields} | Tail], Acc) ->
    FieldsOut = lists:foldl(
        fun ({A, _, _, B, _, _}, TmpAcc) ->
            [{A, B} | TmpAcc];
            (_, TmpAcc) -> TmpAcc
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
    collect_messages(Tail ++ SubMessages, [{Name, FieldsOut} | Acc]).

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

%% @hidden
scan(String) ->
    scan(String, [], 1).

%% @hidden
scan([${|Rest], Accum, Line) ->
    scan(Rest, [{'{', Line}|Accum], Line);
scan([$}|Rest], Accum, Line) ->
    scan(Rest, [{'}', Line}|Accum], Line);
scan([$[|Rest], Accum, Line) ->
    scan(Rest, [{'[', Line}|Accum], Line);
scan([$]|Rest], Accum, Line) ->
    scan(Rest, [{']', Line}|Accum], Line);
scan([$(|Rest], Accum, Line) ->
    scan(Rest, [{'(', Line}|Accum], Line);
scan([$)|Rest], Accum, Line) ->
    scan(Rest, [{')', Line}|Accum], Line);
scan([$=|Rest], Accum, Line) ->
    scan(Rest, [{'=', Line}|Accum], Line);
scan([$;|Rest], Accum, Line) ->
    scan(Rest, [{';', Line}|Accum], Line);
scan([$,|Rest], Accum, Line) ->
    scan(Rest, [{',', Line}|Accum], Line);
scan([Digit|_] = String, Accum, Line)
  when Digit >= $0, Digit =< $9 ->
    {Number, Rest} = scan_number(String),
    scan(Rest, [{number, Line, Number}|Accum], Line);
scan([$-, Digit|_] = String, Accum, Line)
  when Digit >= $0, Digit =< $9 ->
    {Number, Rest} = scan_number(tl(String)),
    scan(Rest, [{number, Line, -Number}|Accum], Line);
scan([$\n|Rest], Accum, Line) ->
    scan(Rest, Accum, Line + 1);
scan([WS|Rest], Accum, Line)
  when WS =:= 32; WS =:= $\t ->
    scan(Rest, Accum, Line);
scan([$/, $/|Rest], Accum, Line) ->
    scan(skip_to_newline(Rest), Accum, Line);
scan([$/, $*|Rest], Accum, Line) ->
    {Rest1, Line1} = skip_comment(Rest, Line),
    scan(Rest1, Accum, Line1);
scan([$"|_] = String, Accum, Line) ->
    {Strval, Rest, Line1} = scan_string(String, Line),
    scan(Rest, [{string, Line, Strval}|Accum], Line1);
scan([C|_] = String, Accum, Line)
  when C >= $A, C =< $Z;
       C >= $a, C =< $z;
       C =:= $_ ->
    {Identifier, Rest} = scan_identifier(String),
    Token = case get_keyword(Identifier) of
        Keyword when is_atom(Keyword) ->
            {Keyword, Line};
        {bareword, Bareword} ->
            {bareword, Line, Bareword}
    end,
    scan(Rest, [Token|Accum], Line);
scan([], Accum, Line) ->
    lists:reverse([{'$end', Line}|Accum]);
scan([C|_], _Accum, Line) ->
    erlang:error({invalid_character, [C], Line}).

%% @hidden
scan_identifier(String) ->
    scan_identifier(String, "").

%% @hidden
scan_identifier([C|Rest], Accum)
  when C >= $A, C =< $Z;
       C >= $a, C =< $z;
       C >= $0, C =< $9;
       C =:= $_;
       C =:= $. ->
    scan_identifier(Rest, [C|Accum]);
scan_identifier(Rest, Accum) ->
    {lists:reverse(Accum), Rest}.

%% @hidden
scan_number(String) ->
    {A, Rest1} = scan_integer(String),
    case Rest1 of
        [$.|Fraction] ->
            {B, Rest2} = scan_identifier(Fraction),
            {A + list_to_float("0." ++ B), Rest2};
        [$e|Exp] ->
            {B, Rest2} = scan_integer(Exp),
            {list_to_float(integer_to_list(A) ++ ".0e" ++ integer_to_list(B)), Rest2};
        [$x|Rest] when A =:= 0 ->
            {Hex, Rest2} = scan_identifier(Rest),
            {erlang:list_to_integer(Hex, 16), Rest2};
        _ ->
            {A, Rest1}
    end.

%% @hidden
scan_integer(String) ->
    scan_integer(String, 0).

%% @hidden
scan_integer([D|Rest], Accum)
  when D >= $0, D =< $9 ->
    scan_integer(Rest, Accum * 10 + (D - $0));
scan_integer(Rest, Accum) ->
    {Accum, Rest}.

%% @hidden
scan_string([$"|String], Line) ->
    scan_string(String, "", Line).

%% @hidden
scan_string([$"|Rest], Accum, Line) ->
    {lists:reverse(Accum), Rest, Line};
scan_string([$\\, $a|Rest], Accum, Line) ->
    scan_string(Rest, [7|Accum], Line);
scan_string([$\\, $e|Rest], Accum, Line) ->
    scan_string(Rest, [$\e|Accum], Line);
scan_string([$\\, $f|Rest], Accum, Line) ->
    scan_string(Rest, [$\f|Accum], Line);
scan_string([$\\, $n|Rest], Accum, Line) ->
    scan_string(Rest, [$\n|Accum], Line);
scan_string([$\\, $r|Rest], Accum, Line) ->
    scan_string(Rest, [$\r|Accum], Line);
scan_string([$\\, $t|Rest], Accum, Line) ->
    scan_string(Rest, [$\t|Accum], Line);
scan_string([$\\, $v|Rest], Accum, Line) ->
    scan_string(Rest, [$\v|Accum], Line);
scan_string([$\\, D1, D2, D3|Rest], Accum, Line)
  when D1 >= $0, D1 =< $7, D2 >= $0, D2 =< $7, D3 >= $0, D3 =< $7 ->
    scan_string(Rest, [erlang:list_to_integer([D1, D2, D3], 8)|Accum], Line);
scan_string([$\\, $x, H1, H2|Rest], Accum, Line) ->
    scan_string(Rest, [erlang:list_to_integer([H1, H2], 16)|Accum], Line);
scan_string([$\\, Char|Rest], Accum, Line) ->
    scan_string(Rest, [Char|Accum], Line);
scan_string([$\n|Rest], Accum, Line) ->
    scan_string(Rest, [$\n|Accum], Line + 1);
scan_string([Char|Rest], Accum, Line) ->
    scan_string(Rest, [Char|Accum], Line).

%% @hidden
skip_to_newline([$\n|Rest]) ->
    Rest;
skip_to_newline([]) ->
    [];
skip_to_newline([_|Rest]) ->
    skip_to_newline(Rest).

%% @hidden
skip_comment([$*, $/|Rest], Line) ->
    {Rest, Line};
skip_comment([$\n|Rest], Line) ->
    skip_comment(Rest, Line + 1);
skip_comment([_|Rest], Line) ->
    skip_comment(Rest, Line).

%% @hidden
get_keyword("import") ->
    import;
get_keyword("package") ->
    package;
get_keyword("option") ->
    option;
get_keyword("message") ->
    message;
get_keyword("group") ->
    group;
get_keyword("enum") ->
    enum;
get_keyword("extend") ->
    extend;
get_keyword("service") ->
    service;
get_keyword("rpc") ->
    rpc;
get_keyword("required") ->
    required;
get_keyword("optional") ->
    optional;
get_keyword("repeated") ->
    repeated;
get_keyword("returns") ->
    returns;
get_keyword("extensions") ->
    extensions;
get_keyword("max") ->
    max;
get_keyword("to") ->
    to;
get_keyword("true") ->
    true;
get_keyword("false") ->
    false;
get_keyword(Bareword) ->
    {bareword, Bareword}.
