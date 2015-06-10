%% Copyright (c) 2009 
%% Nick Gerakines <nick@gerakines.net>
%% Jacob Vorreuter <jacob.vorreuter@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
-module(protobuffs_compile).
-export([scan_file/1, output/2, scan_file_src/1]).

scan_file(ProtoFile) when is_list(ProtoFile) ->
    Basename = filename:basename(ProtoFile, ".proto") ++ "_pb",
    Parsed = protobuffs_parser:parse_file(ProtoFile),
    UntypedMessages = collect_full_messages(Parsed),
    Messages = resolve_types (UntypedMessages),
    output(Basename, Messages).

output(Basename, Messages) ->
    ok = write_header_include_file(Basename, Messages),
    BeamFile = filename:dirname(code:which(?MODULE)) ++ "/pokemon_pb.beam",
	{ok,{_,[{abstract_code,{_,Forms}}]}} =beam_lib:chunks(BeamFile, [abstract_code]),
    Forms1 = filter_forms(Messages, Forms, Basename, []),
    {ok, _, Bytes, _Warnings} = compile:forms(Forms1, [return]),
    file:write_file(Basename ++ ".beam", Bytes).

%%--------------------------------------------------------------------
%% @doc Generats a source .erl file and header file .hrl
%%--------------------------------------------------------------------
scan_file_src(ProtoFile) when is_list(ProtoFile) ->
    Basename = filename:basename(ProtoFile, ".proto") ++ "_pb",
    Parsed = protobuffs_parser:parse_file(ProtoFile),
    UntypedMessages = collect_full_messages(Parsed),
    Messages = resolve_types (UntypedMessages),
    output_src(Basename, Messages).

output_src(Basename, Messages) ->
    ok = write_header_include_file(Basename, Messages),
    BeamFile = filename:dirname(code:which(?MODULE)) ++ "/pokemon_pb.beam",
	{ok,{_,[{abstract_code,{_,Forms}}]}} =beam_lib:chunks(BeamFile, [abstract_code]),
    Forms1 = filter_forms(Messages, Forms, Basename, []),
	SrcFile = Basename ++ ".erl",
	file:write_file(SrcFile, erl_prettypr:format(erl_syntax:form_list(Forms1))).

filter_forms(Msgs, [{attribute,L,file,{_,_}}|Tail], Basename, Acc) ->
    filter_forms(Msgs, Tail, Basename, [{attribute,L,file,{"src/" ++ Basename ++ ".erl",L}}|Acc]);

filter_forms(Msgs, [{attribute,L,module,pokemon_pb}|Tail], Basename, Acc) ->
    filter_forms(Msgs, Tail, Basename, [{attribute,L,module,list_to_atom(Basename)}|Acc]);

filter_forms(Msgs, [{attribute,L,export,[{encode_pikachu,1},{decode_pikachu,1},{encode,1},{decode,2}]}|Tail], Basename, Acc) ->
    Exports = lists:foldl(
        fun({Name,_}, Acc1) ->
            [{list_to_atom("encode_" ++ string:to_lower(Name)),1},
             {list_to_atom("decode_" ++ string:to_lower(Name)),1} | Acc1]
        end, [], Msgs),
    Exports1 = [{encode,1},{encode,2},{decode,2}] ++ Exports,
    filter_forms(Msgs, Tail, Basename, [{attribute,L,export,Exports1}|Acc]);

filter_forms(Msgs, [{attribute,L,record,{pikachu,_}}|Tail], Basename, Acc) ->
    Records = [begin
        OutFields = [string:to_lower(A) || {_, _, _, A, _, _} <- lists:keysort(1, Fields)],
        Frm_Fields = [{record_field,L,{atom,L,list_to_atom(OutField)}}|| OutField <- OutFields],
        {attribute, L, record, {atomize(Name), Frm_Fields}}
     end || {Name, Fields} <- Msgs],
    filter_forms(Msgs, Tail, Basename, Records ++ Acc);

filter_forms(Msgs, [{function,L,encode_pikachu,1,[Clause]}|Tail], Basename, Acc) ->
    Functions = [begin
        {function,L,list_to_atom("encode_" ++ string:to_lower(Name)),1,[replace_atom(Clause, pikachu, atomize(Name))]} 
    end || {Name, _} <- Msgs],
    filter_forms(Msgs, Tail, Basename, Functions ++ Acc);

filter_forms(Msgs, [{function,L,encode,2,[Clause]}|Tail], Basename, Acc) ->
    filter_forms(Msgs, Tail, Basename, [expand_encode_function(Msgs, L, Clause)|Acc]);

filter_forms(Msgs, [{function,L,decode_pikachu,1,[Clause]}|Tail], Basename, Acc) ->
    Functions = [begin
        {function,L,list_to_atom("decode_" ++ string:to_lower(Name)),1,[replace_atom(Clause, pikachu, atomize(Name))]} 
    end || {Name, _} <- Msgs],
    filter_forms(Msgs, Tail, Basename, Functions ++ Acc);

filter_forms(Msgs, [{function,L,decode,2,[Clause]}|Tail], Basename, Acc) ->
    filter_forms(Msgs, Tail, Basename, [expand_decode_function(Msgs, L, Clause)|Acc]);

filter_forms(Msgs, [{function,L,to_record,2,[Clause]}|Tail], Basename, Acc) ->
    filter_forms(Msgs, Tail, Basename, [expand_to_record_function(Msgs, L, Clause)|Acc]);

filter_forms(Msgs, [Form|Tail], Basename, Acc) ->
    filter_forms(Msgs, Tail, Basename, [Form|Acc]);

filter_forms(_, [], _, Acc) -> lists:reverse(Acc).

expand_encode_function(Msgs, Line, Clause) ->
    {function,Line,encode,2,[filter_encode_clause(Msg, Clause) || Msg <- Msgs]}.

filter_encode_clause({MsgName, Fields}, {clause,L,_Args,Guards,_Content}) ->
    Cons = lists:foldl(
        fun({FNum,Tag,SType,SName,_,Default}, Acc) ->
            {cons,L,
                {call,L,{atom,L,pack},[
                    {integer,L,FNum},
                    {atom,L,Tag},
                    {call,L,{atom,L,with_default},[{record_field,L,{var,L,'_Record'},atomize(MsgName),{atom,L,atomize(SName)}},erl_parse:abstract(Default)]},
                    {atom,L,atomize(SType)},
                    {nil,L}]},
                Acc}
        end, {nil,L}, Fields),
    ToBin = {call,L,{atom,L,iolist_to_binary},[Cons]},
    {clause,L,[{atom,L,atomize(MsgName)},{var,L,'_Record'}],Guards,[ToBin]}.

expand_decode_function(Msgs, Line, Clause) ->
    {function,Line,decode,2,[filter_decode_clause(Msgs, Msg, Clause) || Msg <- Msgs]}.

filter_decode_clause(Msgs, {MsgName, Fields}, {clause,L,_Args,Guards,[_,B,C]}) ->
    Types = lists:keysort(1, [{FNum, atomize(SName), atomize(SType), decode_opts(Msgs, Tag, SType)} || {FNum,Tag,SType,SName,_,_} <- Fields]),
    Cons = lists:foldl(
        fun({FNum, FName, Type, Opts}, Acc) ->
            {cons,L,{tuple,L,[{integer,L,FNum},{atom,L,FName},{atom,L,Type},erl_parse:abstract(Opts)]},Acc}
        end, {nil,L}, Types),
    A = {match,L,{var,L,'Types'},Cons},
    C1 = replace_atom(C, pikachu, atomize(MsgName)),
    {clause,L,[{atom,L,atomize(MsgName)},{var,L,'Bytes'}],Guards,[A,B,C1]}.

decode_opts(Msgs, Tag, Type) ->
    Opts0 = if Tag == repeated -> [repeated]; true -> [] end,
    case lists:keymember(Type, 1, Msgs) of
        true ->
            [is_record|Opts0];
        false ->
            Opts0
    end.

expand_to_record_function(Msgs, Line, Clause) ->
    {function,Line,to_record,2,[filter_to_record_clause(Msg, Clause) || Msg <- Msgs]}.

filter_to_record_clause({MsgName, _}, {clause,L,[_Param1,Param2],Guards,[Fold]}) ->
    Fold1 = replace_atom(Fold, pikachu, atomize(MsgName)),
    {clause,L,[{atom,L,atomize(MsgName)},Param2],Guards,[Fold1]}.

%% [{"Location",
%%   [{2,required,"string","country",number,none},
%%    {1,required,"string","region",number,none}]},
%%  {"Person",
%%   [{5,optional,"Location","location",number,none},
%%    {4,required,"int32","age",number,none},
%%    {3,required,"string","phone_number",number,none},
%%    {2,required,"string","address",number,none},
%%    {1,required,"string","name",number,none}]}]
collect_full_messages(Data) -> collect_full_messages(Data, []).
collect_full_messages([{message, Name, Fields} | Tail], Acc) ->
  ListName = case erlang:is_list (hd(Name)) of
               true -> Name;
               false -> [Name]
             end,

  FieldsOut = lists:foldl(
    fun (Input, TmpAcc) ->
        case Input of
          {_, _, _, _, _, _} ->  [Input | TmpAcc];
          _ -> TmpAcc
        end
    end, [], Fields),

  SubMessages = lists:foldl(
    fun ({message, C, D}, TmpAcc) -> [{message, [C | ListName], D} | TmpAcc];
      (_, TmpAcc) -> TmpAcc
    end, [], Fields),

  collect_full_messages(Tail ++ SubMessages, [{ListName, FieldsOut} | Acc]);
collect_full_messages([{package, _Line1},
                       {bareword, _Line2, _PackageName},
                       {';', _Line3} | Tail], Acc) ->
    collect_full_messages(Tail, Acc);
collect_full_messages([], Acc) -> 
    Acc.

resolve_types (Data) -> resolve_types (Data, Data, []).
resolve_types ([{TypePath, Fields} | Tail], AllPaths, Acc) ->
  FieldsOut = lists:foldl(
      fun (Input, TmpAcc) ->
          case Input of
              {Index, Rules, Type, Identifier, RealType, Other} ->
                case is_scalar_type (Type) of
                  true -> [Input | TmpAcc];
                  false ->
                    PossiblePaths =
                      case string:tokens (Type,".") of
                        [Type] ->
                          all_possible_type_paths (Type, TypePath);
                        FullPath ->
                          % handle types of the form Foo.Bar which are absolute,
                          % so we just convert to a type path and check it.
                          [lists:reverse (FullPath)]
                      end,
                    RealPath =
                      case find_type (PossiblePaths, AllPaths) of
                        false ->
                          throw (["Unknown Type ", Type]);
                        ResultType ->
                          ResultType
                      end,
                    [{Index, Rules, type_path_to_type (RealPath), Identifier, RealType, Other} | TmpAcc]
                end;
              _ -> TmpAcc
          end
      end, [], Fields),
  resolve_types (Tail, AllPaths,
                 [ {type_path_to_type (TypePath), lists:reverse (FieldsOut) } | Acc]);
resolve_types ([], _, Acc) ->
    Acc.

write_header_include_file(Basename, Messages) ->
    {ok, FileRef} = file:open(Basename ++ ".hrl", [write]),
    [begin
        OutFields = [string:to_lower(A) || {_, _, _, A, _, _} <- lists:keysort(1, Fields)],
        io:format(FileRef, "-record(~s, {~s}).~n", [string:to_lower(Name), string:join(OutFields, ", ")])
    end || {Name, Fields} <- Messages],
    file:close(FileRef).

atomize(String) ->
    list_to_atom(string:to_lower(String)).

replace_atom(Find, Find, Replace) -> Replace;

replace_atom(Tuple, Find, Replace) when is_tuple(Tuple) ->
    list_to_tuple([replace_atom(Term, Find, Replace) || Term <- tuple_to_list(Tuple)]);

replace_atom(List, Find, Replace) when is_list(List) ->
    [replace_atom(Term, Find, Replace) || Term <- List];

replace_atom(Other, _Find, _Replace) ->
    Other.

is_scalar_type ("double") -> true;
is_scalar_type ("float") -> true;
is_scalar_type ("int32") -> true;
is_scalar_type ("int64") -> true;
is_scalar_type ("uint32") -> true;
is_scalar_type ("uint64") -> true;
is_scalar_type ("sint32") -> true;
is_scalar_type ("sint64") -> true;
is_scalar_type ("fixed32") -> true;
is_scalar_type ("fixed64") -> true;
is_scalar_type ("sfixed32") -> true;
is_scalar_type ("sfixed64") -> true;
is_scalar_type ("bool") -> true;
is_scalar_type ("string") -> true;
is_scalar_type ("bytes") -> true;
is_scalar_type (_) -> false.

sublists(List) when is_list(List) ->
  sublists(List,[]).
sublists([],Acc) ->
  [ [] | Acc ];
sublists(List,Acc) ->
  sublists (tl (List), [ List | Acc ]).

all_possible_type_paths (Type, TypePath) ->
  lists:foldl (fun (TypeSuffix, AccIn) ->
                 [[Type | TypeSuffix] | AccIn]
               end,
               [],
               sublists (TypePath)).

find_type ([], _KnownTypes) ->
  false;
find_type ([Type | TailTypes], KnownTypes) ->
  case lists:keysearch (Type, 1, KnownTypes) of
    false ->
      find_type (TailTypes, KnownTypes);
    {value, {RealType, _}} ->
      RealType
  end.

type_path_to_type (TypePath) ->
  string:join (lists:reverse (TypePath), "_").

