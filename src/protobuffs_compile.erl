%%%%	== SAMPLE PROTO RECORDS ==
%%%%
%%%%    message Person {
%%%%   	required string name = 1;
%%%%   	required string address = 2;
%%%%   	required string phone_number = 3 [default = "+1 (000) 000-0000"];
%%%%   	required int32 age = 4 [default = 25];
%%%%   	repeated string hobbies = 5;
%%%%   	repeated Location locations = 6;
%%%%   	required Title title = 7;
%%%%    }
%%%%   
%%%%    message Location {
%%%%     required string region = 1;
%%%%     required string country = 2
%%%%    }
%%%%   
%%%%    message Title {
%%%%     required string employer;
%%%%     required string position
%%%%    }
%%
%%		== CODE EQUIVALENT TO FORMS GENERATED FROM SAMPLE PROTO MESSAGES
%%
%%		-module(encode).
%%		-export([encode_person/1]).
%%		
%%		-record(person, {name, address, phone_number, age, hobbies, locations, title}).
%%		-record(location, {region, country}).
%%		-record(title, {employer, position}).
%%		
%%		encode_person(Rec) ->
%%			iolist_to_binary([
%%				pack_person(1, required, with_default(Rec#person.name, none), atomize_type("string"), []),
%%				pack_person(2, required, with_default(Rec#person.address, none), atomize_type("string"), []),
%%				pack_person(3, required, with_default(Rec#person.phone_number, "+1 (000) 000-0000"), atomize_type("string"), []),
%%				pack_person(4, required, with_default(Rec#person.age, 25), atomize_type("int32"), []),
%%				pack_person(5, repeated, with_default(Rec#person.hobbies, none), atomize_type("string"), []),
%%				pack_person(6, repeated, with_default(Rec#person.locations, none), atomize_type("Location"), []),
%%				pack_person(7, required, with_default(Rec#person.title, none), atomize_type("Title"), [])
%%			]).
%%				
%%		with_default(undefined, none) -> undefined;
%%		with_default(undefined, Default) -> Default;
%%		with_default(Val, _) -> Val.
%%		
%%		atomize_type(Type) -> list_to_atom(string:to_lower(Type)).
%%		
%%		pack_person(_, optional, undefined, _, _) -> [];
%%		
%%		pack_person(_, required, undefined, _, _) ->
%%			exit(required_field_is_undefined);
%%			
%%		pack_person(1, required, Data, string, _) when is_list(Data) ->
%%			protobuffs:encode(1, Data, string);
%%		
%%		pack_person(2, required, Data, string, _) when is_list(Data) ->
%%			protobuffs:encode(1, Data, string);
%%				
%%		pack_person(3, required, Data, string, _) when is_list(Data) ->
%%			protobuffs:encode(1, Data, string);
%%			
%%		pack_person(4, required, Data, int32, _) when is_integer(Data) ->
%%			protobuffs:encode(2, Data, int32);
%%		
%%		pack_person(5, repeated, [], string, Acc) -> lists:reverse(Acc);
%%		pack_person(5, repeated, [Head|Tail], string, Acc) when is_list(Head) ->
%%			Acc1 = [protobuffs:encode(5, Head, string) | Acc],
%%			pack_person(5, repeated, Tail, string, Acc1);
%%					
%%		pack_person(6, repeated, [], location, Acc) -> lists:reverse(Acc);
%%		pack_person(6, repeated, [Head|Tail], location, Acc) when is_record(Head, location) ->
%%			Acc1 = [protobuffs:encode(6, encode_location(Head), bytes) | Acc],
%%			pack_person(6, repeated, Tail, location, Acc1);
%%			
%%		pack_person(7, required, Data, title, _) when is_record(Data, title) ->
%%			protobuffs:encode(7, encode_title(Data), bytes).
%%			
%%		encode_location(_Data) ->
%%			[]. % ...
%%			
%%		encode_title(_Data) ->
%%			[]. % ...
%%	
-module(protobuffs_compile).
-author('jacob.vorreuter@gmail.com').
-export([scan_file/1]).

-import(forms_helper, [init_state/0,new_line/0,line/0,append/1,fetch/0,a/1]).

%% @spec scan_file(File_Path) -> ok | {'EXIT', atom()}	
%%		File_Path = string()
scan_file(File_Path) ->
	Basename = filename:basename(File_Path, ".proto") ++ "_pb",
    Parsed = protobuffs_parser:parse_file(File_Path),
	Messages = collect_full_messages(Parsed),

	ok = init_state(),
		
	ok = write_module_declarations(Basename, Messages),
	
	ok = write_headers(Basename, Messages),
	
	ok = write_message_functions(Basename, Messages),
	
	Forms = fetch(),
	
	%error_logger:info_msg("forms: ~p~n", [Forms]),
	
	case compile:forms(Forms) of
		{ok, _, Bytes} ->
			ok = file:write_file(Basename ++ ".beam", Bytes);
		Error ->
			error_logger:error_msg("compilation failed: ~p~n", [Error])
	end,
	
	ok.

%% @hidden
write_headers(Basename, Messages) ->
	append({attribute,1,file,{"./" ++ Basename ++ ".hrl",1}}),
	
	{ok, FileRef} = file:open(Basename ++ ".hrl", [write]),
    lists:foldl(
        fun({Name, Fields}, LineNum) ->	
            OutFields = [string:to_lower(A) || {_, _, _, A, _, _} <- lists:keysort(1, Fields)],
            io:format(FileRef, "-record(~s, {~s}).~n", [string:to_lower(Name), string:join(OutFields, ", ")]),
			Frm_Fields = [{record_field,LineNum,{atom,LineNum,list_to_atom(OutField)}}|| OutField <- OutFields],
			append({attribute, LineNum, record, {list_to_atom(string:to_lower(Name)), Frm_Fields}}),
			LineNum+1
        end,
        1, Messages),
    ok = file:close(FileRef),

	append({attribute,new_line(),file,{Basename ++ ".erl",line()}}),
	
	ok.	
	
%% @hidden
write_module_declarations(Basename, Messages) ->
	append({attribute,line(),file,{Basename ++ ".erl",line()}}),
	append({attribute,line(),module,list_to_atom(Basename)}),
	append({attribute,new_line(),export, exports(Messages)}),
	ok.

%% @hidden	
exports(Messages) ->
	lists:foldl(
		fun({RecName, _}, Acc) ->
			LName = string:to_lower(RecName),
			EAtom = list_to_atom("encode_" ++ LName),
			DAtom = list_to_atom("decode_" ++ LName),
			[{EAtom,1}, {DAtom,1}|Acc]
		end,
	[], Messages).		
		
%% @hidden	
%% @spec write_message_functions([{RecName, Fields}|Last]) -> ok
%%		RecName = string()
%%		Fields = [Field_Tuple|_]
%% 		Field_Tuple = {Pos::integer(), Tag::atom(), Type::string(), Name::string(), number, Default::any()|none}
write_message_functions(Basename, [{RecName, Fields}|Last]) ->
	%error_logger:info_msg("fields: ~p~n", [Fields]),
	write_encode(Basename, RecName, Fields),
	write_decode(Basename, RecName, Fields),
	write_message_functions(Basename, Last);
write_message_functions(_, []) -> ok.

%% @hidden
%% @spec write_encode(Basename, RecName, Fields) -> ok
%%		Basename = string()
%%		RecName = string()
%%		Fields = [Field_Tuple|_]
%% 		Field_Tuple = {Pos::integer(), Tag::atom(), Type::string(), Name::string(), number, Default::any()|none}	
write_encode(Basename, RecName, Fields) ->
	LRecName = string:to_lower(RecName),
	append({function,new_line(),list_to_atom("encode_" ++ LRecName),1,
              [{clause,line(),
                       [{var,line(),'Rec'}],
					   [],
                       [{call,new_line(),
					     {atom,line(),iolist_to_binary},
					     [
							lists:foldl(
								fun({Pos,Tag,Type,Name,_,Default}, Acc) ->
									T = {call,new_line(),
									        {atom,line(),list_to_atom("pack_" ++ LRecName)},
									        [{integer,line(),Pos},
											 {atom,line(),Tag},
									         {call,line(),
									          {atom,line(),list_to_atom("with_default_" ++ LRecName)},[
												{record_field,line(),{var,line(),'Rec'},list_to_atom(LRecName),{atom,line(),list_to_atom(Name)}}, 
												a(Default)]},
									         {call,line(),{atom,line(),list_to_atom("atomize_type" ++ LRecName)},[{string,line(),Type}]},
									         {nil,line()}]},
									{cons,line(),T,Acc}
								end, {nil,line()}, Fields)
						 ]}]}]}),
						
	append({function,new_line(),list_to_atom("with_default_" ++ LRecName),2,
			 [{clause,line(),
				[{atom,line(),undefined},{atom,line(),none}],
				[],
				[{atom,line(),undefined}]},
			  {clause,new_line(),
			   	[{atom,line(),undefined},{var,line(),'Default'}],
			   	[],
			   	[{var,line(),'Default'}]},
			  {clause,new_line(),
				[{var,line(),'Val'},{var,line(),'_'}],
				[],
				[{var,line(),'Val'}]}]}),
						
	append({function,new_line(),list_to_atom("atomize_type" ++ LRecName),1,
			 [{clause,line(),
			   [{var,line(),'Type'}],
			   [],
			   [{call,line(),
			     {atom,line(),list_to_atom},
			     [{call,line(),
			       {remote,line(),{atom,line(),string},{atom,line(),to_lower}},
			       [{var,line(),'Type'}]}]}]}]}),
			
	append({function,new_line(),list_to_atom("pack_" ++ LRecName),5,
			lists:reverse(lists:foldl(
				fun({Pos,Tag,Type,_,_,_}, Acc) ->
					case [Tag, Type] of
						[repeated, [C|_]] when C >= $A, C =< $Z ->
							LRecFieldName = string:to_lower(Type),							

							[{clause,new_line(),
							   [{integer,line(),Pos},
								{atom,line(),repeated},
							    {cons,line(),{var,line(),'Head'},{var,line(),'Tail'}},
							    {atom,line(),list_to_atom(LRecFieldName)},
							    {var,line(),'Acc'}],
							   [[{call,line(),{atom,line(),is_record},[{var,line(),'Head'},{atom,line(),list_to_atom(LRecFieldName)}]}]],
							   [{match,new_line(),
							      {var,line(),'Acc1'},
							      {cons,line(),
							       {call,line(),
							        {remote,line(),{atom,line(),protobuffs},{atom,line(),encode}},
							        [{integer,line(),Pos},
							         {call,line(),
							          {atom,line(),list_to_atom("encode_" ++ LRecFieldName)},
							          [{var,line(),'Head'}]},
							         {atom,line(),bytes}]},
							       {var,line(),'Acc'}}},
							     {call,new_line(),
							      {atom,line(),list_to_atom("pack_" ++ LRecName)},
							      [{integer,line(),Pos},{atom,line(),repeated},{var,line(),'Tail'},{atom,line(),list_to_atom(LRecFieldName)},{var,line(),'Acc1'}]}]},
							
							{clause,new_line(),
							   [{integer,line(),Pos},{atom,line(),repeated},{nil,line()},{atom,line(),list_to_atom(LRecFieldName)},{var,line(),'Acc'}],
							   [],
							   [{call,line(),{remote,line(),{atom,line(),lists},{atom,line(),reverse}},[{var,line(),'Acc'}]}]} | Acc];

						[repeated, _] ->

							[{clause,new_line(),
							   [{integer,line(),Pos},
								{atom,line(),repeated},
							    {cons,line(),{var,line(),'Head'},{var,line(),'Tail'}},
							    {atom,line(),list_to_atom(Type)},
							    {var,line(),'Acc'}],
							   [case Type of
										"string" ->
											[{call,line(),{atom,line(),is_list},[{var,line(),'Head'}]}];
										"int" ++ _ ->
											[{call,line(),{atom,line(),is_integer},[{var,line(),'Head'}]}];
										"bytes" ->
											[{call,line(),{atom,line(),is_binary},[{var,line(),'Head'}]}];
										_ -> []
								end],
							   [{match,new_line(),
							     {var,line(),'Acc1'},
							     {cons,line(),
							      {call,line(),
							       {remote,line(),{atom,line(),protobuffs},{atom,line(),encode}},
							       [{integer,line(),Pos},{var,line(),'Head'},{atom,line(),list_to_atom(Type)}]},
							      {var,line(),'Acc'}}},
							    {call,new_line(),
							     {atom,line(),list_to_atom("pack_" ++ LRecName)},
							     [{integer,line(),Pos},{atom,line(),repeated},{var,line(),'Tail'},{atom,line(),list_to_atom(Type)},{var,line(),'Acc1'}]}]},

							{clause,new_line(),
							   [{integer,line(),Pos},{atom,line(),repeated},{nil,line()},{atom,line(),list_to_atom(Type)},{var,line(),'Acc'}],
							   [],
							   [{call,line(),{remote,line(),{atom,line(),lists},{atom,line(),reverse}},[{var,line(),'Acc'}]}]} | Acc];
							
						[_, [C|_]] when C >= $A, C =< $Z -> 
							LRecFieldName = string:to_lower(Type),
							[{clause,new_line(),
							    [{integer,line(),Pos},{atom,line(),Tag},{var,line(),'Data'},{atom,line(),list_to_atom(LRecFieldName)},{var,line(),'_'}],
							    [[{call,line(),{atom,line(),is_record},[{var,line(),'Data'},{atom,line(),list_to_atom(LRecFieldName)}]}]],
							    [{call,new_line(),
							      {remote,line(),{atom,line(),protobuffs},{atom,line(),encode}},
							      [{integer,line(),Pos},
							       {call,line(),
							        {remote,line(),{atom,line(),list_to_atom(string:to_lower(Basename))},{atom,line(),list_to_atom("encode_" ++ LRecFieldName)}},
							        [{var,line(),'Data'}]},
							       {atom,line(),bytes}]}]} | Acc];

						[_, _] ->
							[{clause,new_line(),
								[{integer,line(),Pos},{atom,line(),Tag},{var,line(),'Data'},{atom,line(),list_to_atom(Type)},{var,line(),'_'}],
								   [case Type of
										"string" ->
											[{call,line(),{atom,line(),is_list},[{var,line(),'Data'}]}];
										"int" ++ _ ->
											[{call,line(),{atom,line(),is_integer},[{var,line(),'Data'}]}];
										"bytes" ->
											[{call,line(),{atom,line(),is_binary},[{var,line(),'Data'}]}];
										_ -> []
									end],
								   [{call,new_line(),
								     {remote,line(),{atom,line(),protobuffs},{atom,line(),encode}},
								     [{integer,line(),Pos},{var,line(),'Data'},{atom,line(),list_to_atom(Type)}]}]} | Acc]
								
					end
				end, 
				[{clause,new_line(),
				   [{var,line(),'_'},{atom,line(),required},{atom,line(),undefined},{var,line(),'_'},{var,line(),'_'}],
				   [],
				   [{call,line(),{atom,line(),exit},[{atom,line(),required_field_is_undefined}]}]},
				 {clause,new_line(),
				   [{var,line(),'_'},{atom,line(),optional},{atom,line(),undefined},{var,line(),'_'},{var,line(),'_'}],
				   [],
				   [{nil,line()}]}], 
					
				Fields))}),
	
	ok.
		
%% @hidden	
%% @spec write_decode(Basename, RecName, Fields) -> ok
%%		Basename = string()
%%		RecName = string()
%%		Fields = [Field_Tuple|_]
%% 		Field_Tuple = {Pos::integer(), Tag::atom(), Type::string(), Name::string(), number, Default::any()|none}	
write_decode(Basename, RecName, Fields) ->
	LRecName = string:to_lower(RecName),

	append({function,new_line(),list_to_atom("decode_" ++ LRecName),1,
		     [{clause,line(),
		          [{var,line(),'Bytes'}],
		          [[{call,line(),{atom,line(),is_binary},[{var,line(),'Bytes'}]}]],
		          [{match,new_line(),
		               {var,line(),'Data_Tuples'},
		               {call,line(),
		                   {remote,line(),{atom,line(),protobuffs},{atom,line(),decode_many}},
		                   [{var,line(),'Bytes'}]}},
		           {call,new_line(),{atom,line(),list_to_atom(LRecName ++ "_to_record")},[{var,line(),'Data_Tuples'}]}]}]}),	

	append({function,new_line(),list_to_atom(LRecName ++ "_to_record"),1,
	     [{clause,line(),
	          [{var,line(),'Data_Tuples'}],
	          [],
	          [{record,new_line(),list_to_atom(LRecName),
	               [begin
					 Func_Get_Value =
						case Tag of
							repeated -> list_to_atom("get_values_" ++ LRecName);
							_ -> list_to_atom("get_value_" ++ LRecName)
						end,
					 Default_Type =
						case Default of
							A when is_atom(A) -> atom;
							A when is_list(A) -> string;
							A when is_integer(A) -> integer
						end,
					 {record_field,new_line(),
	                    {atom,line(),list_to_atom(Name)},
	                    {call,line(),
	                        {atom,line(),list_to_atom("unpack_" ++ LRecName)},
	                        [{call,line(),
	                             {atom,line(),Func_Get_Value},
	                             [{var,line(),'Data_Tuples'},
	                              {integer,line(),Pos},
	                              {Default_Type,line(),Default}]},
	                         {integer,line(),Pos},
	                         {string,line(),Type}]}}
					end || {Pos,Tag,Type,Name,_,Default} <- Fields]}]}]}),
					
	append({function,new_line(),list_to_atom("get_value_" ++ LRecName),3,
	     [{clause,line(),
	          [{var,line(),'Data_Tuples'},{var,line(),'Pos'},{var,line(),'Default'}],
	          [],
	          [{'case',new_line(),
	               {call,line(),
	                   {remote,line(),{atom,line(),proplists},{atom,line(),get_value}},
	                   [{var,line(),'Pos'},{var,line(),'Data_Tuples'}]},
	               [{clause,new_line(),[{atom,line(),undefined}],[],[{var,line(),'Default'}]},
	                {clause,new_line(),[{var,line(),'Value'}],[],[{var,line(),'Value'}]}]}]}]}),

	append({function,new_line(),list_to_atom("get_values_" ++ LRecName),3,
	     [{clause,line(),
	          [{var,line(),'Data_Tuples'},{var,line(),'Pos'},{var,line(),'Default'}],
	          [],
	          [{'case',new_line(),
	               {call,line(),
	                   {remote,line(),{atom,line(),proplists},{atom,line(),get_all_values}},
	                   [{var,line(),'Pos'},{var,line(),'Data_Tuples'}]},
	               [{clause,new_line(),[{nil,line()}],[],[{var,line(),'Default'}]},
	                {clause,new_line(),[{var,line(),'Values'}],[],[{var,line(),'Values'}]}]}]}]}),
	
	append({function,new_line(),list_to_atom("unpack_" ++ LRecName),3,
		 [begin
			{clause,line(),
		          [{var,line(),'Data'},{integer,line(),Pos},{string,line(),Type}],
		          [],
				  [case [Tag, Type] of
					
					[repeated, [C|_]] when C >= $A, C =< $Z -> %% repeated record
						{call,new_line(),
						      {remote,line(),{atom,line(),lists},{atom,line(),foldl}},
						      [{'fun',line(),
						        {clauses,
						         [{clause,line(),
						           [{var,line(),'Item'},
						            {var,line(),'Acc'}],
						           [],
						           [{cons,new_line(),
											{call,line(),
												{atom,line(),list_to_atom("decode_" ++ string:to_lower(Type))},
												[{var,line(),'Item'}]},
											{var,43,'Acc'}}]
									}]}},
								{nil,line()},
						        {var,line(),'Data'}]
							};

					[repeated, _] -> %% repeated scalar
						{call,new_line(),
						      {remote,line(),{atom,line(),lists},{atom,line(),foldl}},
						      [{'fun',line(),
						        {clauses,
						         [{clause,line(),
						           [{var,line(),'Item'},
						            {var,line(),'Acc'}],
						           [],
						           [case Type of
										"string" ->
											{cons,new_line(),
												{call,new_line(),
												     {atom,line(),binary_to_list},
												     [{var,line(),'Item'}]},
												{var,43,'Acc'}};
										_ ->
											{cons,new_line(),
												{var,line(),'Item'},
												{var,line(),'Acc'}}
									end]
								  }]}},
						       {nil,line()},
						       {var,line(),'Data'}]};
						
					[_, [C|_]] when C >= $A, C =< $Z -> %% non-repeating record
						{call,new_line(),
						     {atom,line(),apply},
						     [{atom,line(),list_to_atom(string:to_lower(Basename))},
						      {atom,line(),list_to_atom("decode_" ++ string:to_lower(Type))},
						      {cons,line(),{var,line(),'Data'},{nil,line()}}]};
						
					[_, "string"] -> %% non-repeating string
						{call,new_line(),
						     {atom,line(),binary_to_list},
						     [{var,line(),'Data'}]};
					[_, _] ->
						{var,new_line(),'Data'}
						
				  end] }
		 end || {Pos,Tag,Type,_,_,_} <- Fields]}),
		
	ok.

%% @hidden
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