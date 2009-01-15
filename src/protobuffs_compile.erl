-module(protobuffs_compile).
-export([scan_file/1]).

-import(forms_helper, [init_state/0,new_line/0,line/0,append/1,fetch/0]).

%% Field_Tuple = {Pos::integer(), Tag::atom(), Type::string(), Name::string(), number, Default::any()|none}
	
scan_file(File_Path) ->
	Basename = filename:basename(File_Path, ".proto") ++ "_pb",
    Parsed = protobuffs_parser:parse_file(File_Path),
	Messages = protobuffs_parser:collect_full_messages(Parsed),
	io:format("messages: ~p~n", [Messages]),

	ok = init_state(),
		
	ok = write_module_declarations(Basename, Messages),
	
	ok = write_headers(Basename, Messages),
	
	ok = write_message_functions(Basename, Messages),
	
	Forms = fetch(),
	
	io:format("forms: ~p~n", [Forms]),
	
	case compile:forms(Forms) of
		{ok, _, Bytes} ->
			ok = file:write_file(Basename ++ ".beam", Bytes);
		Error ->
			io:format("compilation failed: ~p~n", [Error])
	end,
	
	ok.

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
	
write_module_declarations(Basename, Messages) ->
	append({attribute,line(),file,{Basename ++ ".erl",line()}}),
	append({attribute,line(),module,list_to_atom(Basename)}),
	append({attribute,new_line(),export, exports(Messages)}),
	ok.
	
exports(Messages) ->
	lists:foldl(
		fun({RecName, _}, Acc) ->
			LName = string:to_lower(RecName),
			EAtom = list_to_atom("encode_" ++ LName),
			DAtom = list_to_atom("decode_" ++ LName),
			[{EAtom,1}, {DAtom,1}|Acc]
		end,
	[], Messages).		
			
%% @spec write_message_functions([{RecName, Fields}|Last]) -> ok
%%		RecName = string()
%%		Fields = [Field_Tuple|_]
write_message_functions(Basename, [{RecName, Fields}|Last]) ->
	io:format("fields: ~p~n", [Fields]),
	write_encode(Basename, RecName, Fields),
	write_decode(Basename, RecName, Fields),
	write_message_functions(Basename, Last);
write_message_functions(_, []) -> ok.
	
write_encode(Basename, RecName, Fields) ->
	ModNameAtom = list_to_atom(string:to_lower(Basename)),
	FunctionName = list_to_atom("encode_" ++ string:to_lower(RecName)),
	append({function,new_line(),FunctionName,1,
              [{clause,line(),
                       [{var,line(),'Rec'}],
                       [],
                       [{match,new_line(),
							 {var,line(),'Data_Tuples'},
							  	lists:foldl(
							  		fun({Pos,Tag,Type,Name,_,Default}, Acc) ->
							  			T = {cons,line(),
										 	 {tuple,line(),
										      [{integer,line(),Pos},
										       {atom,line(),Tag},
										       {'case',new_line(),
										         {cons,line(),
													{record_field,line(),{var,line(),'Rec'},list_to_atom(string:to_lower(RecName)),{atom,line(),list_to_atom(Name)}},
													{cons,line(),value_to_tuple(Default),{nil,line()}}},
												       [{clause,new_line(),
												         [{cons,line(),{atom,line(),undefined},{cons,line(),{atom,line(),none},{nil,line()}}}],
												         [],
												         [{atom,line(),undefined}]},
												        {clause,new_line(),
												         [{cons,line(),{atom,line(),undefined},{cons,line(),{var,line(),'_'},{nil,line()}}}],
												         [],
												         [value_to_tuple(Default)]},
												        {clause,new_line(),
												         [{cons,line(),{var,line(),'_'},{cons,line(),{var,line(),'_'},{nil,line()}}}],
												         [],
												         [{record_field,line(),{var,line(),'Rec'},list_to_atom(string:to_lower(RecName)),{atom,line(),list_to_atom(Name)}}]}]
												},
										       {atom,line(),list_to_atom(string:to_lower(Type))}
											  ]}},
							  			erlang:append_element(T, Acc)
							  		end,
							  	{nil,line()}, lists:reverse(Fields))},
						 {call,new_line(),{atom,line(),iolist_to_binary},[
							{call,new_line(),
						      {remote,line(),{atom,line(),lists},{atom,line(),foldl}},
						      [{'fun',new_line(),
						        {clauses,
						         [{clause,line(),
						           [{tuple,line(),
						             [{var,line(),'Pos'},{var,line(),'_Tag'},{var,line(),'Data'},{var,line(),'Type'}]},
						            {var,line(),'Acc'}],
						           [[{call,line(),{atom,line(),is_binary},[{var,line(),'Data'}]},
						             {op,line(),'=/=',{var,line(),'Type'},{atom,line(),bytes}}]],
						           [{cons,new_line(),
						             {call,line(),
						              {remote,line(),{atom,line(),protobuffs},{atom,line(),encode}},
						              [{var,line(),'Pos'},{var,line(),'Data'},{atom,line(),bytes}]},
						             {var,line(),'Acc'}}]},
						          {clause,new_line(),
						           [{tuple,line(),
						             [{var,line(),'Pos'},
						              {var,line(),'_Tag'},
						              {var,line(),'Data'},
						              {var,line(),'_Type'}]},
						            {var,line(),'Acc'}],
						           [[{call,line(),{atom,line(),is_tuple},[{var,line(),'Data'}]}]],
						           [{match,new_line(),
						             {var,line(),'RecName'},
						             {call,line(),
						              {remote,line(),{atom,line(),erlang},{atom,line(),element}},
						              [{integer,line(),1},{var,line(),'Data'}]}},
						            {match,new_line(),
						             {var,line(),'Method'},
						             {call,line(),
						              {atom,line(),list_to_atom},
						              [{op,line(),'++',
						                {string,line(),"encode_"},
						                {call,line(),{atom,line(),atom_to_list},[{var,line(),'RecName'}]}}]}},
						            {match,new_line(),
						             {var,line(),'EncodedRec'},
						             {call,line(),
						              {atom,line(),apply},
						              [{atom,line(),ModNameAtom},
						               {var,line(),'Method'},
						               {cons,line(),{var,line(),'Data'},{nil,line()}}]}},
						            {cons,new_line(),
						             {call,line(),
						              {remote,line(),{atom,line(),protobuffs},{atom,line(),encode}},
						              [{var,line(),'Pos'},{var,line(),'EncodedRec'},{atom,line(),bytes}]},
						             {var,line(),'Acc'}}]},
						          {clause,new_line(),
						           [{tuple,line(),
						             [{var,line(),'Pos'},
						              {atom,line(),repeated},
						              {match,line(),
						               {cons,line(),{var,line(),'Head'},{var,line(),'_'}},
						               {var,line(),'Data'}},
						              {var,line(),'_Type'}]},
						            {var,line(),'Acc'}],
						           [[{call,line(),{atom,line(),is_tuple},[{var,line(),'Head'}]}]],
						           [{match,new_line(),
						             {var,line(),'RecName'},
						             {call,line(),
						              {remote,line(),{atom,line(),erlang},{atom,line(),element}},
						              [{integer,line(),1},{var,line(),'Head'}]}},
						            {match,new_line(),
						             {var,line(),'EncodedRecs'},
						             {call,new_line(),
						              {atom,line(),list_to_binary},
						              [{lc,line(),
						                {block,line(),
						                 [{match,new_line(),
						                   {var,line(),'Method'},
						                   {call,line(),
						                    {atom,line(),list_to_atom},
						                    [{op,line(),'++',
						                      {string,line(),"encode_"},
						                      {call,line(),
						                       {atom,line(),atom_to_list},
						                       [{var,line(),'RecName'}]}}]}},
						                  {match,new_line(),
						                   {var,line(),'EncodedRec'},
						                   {call,line(),
						                    {atom,line(),apply},
						                    [{atom,line(),ModNameAtom},
						                     {var,line(),'Method'},
						                     {cons,line(),{var,line(),'Record'},{nil,line()}}]}},
						                  {call,new_line(),
						                   {remote,line(),{atom,line(),protobuffs},{atom,line(),encode}},
						                   [{var,line(),'Pos'},{var,line(),'EncodedRec'},{atom,line(),bytes}]}]},
						                [{generate,new_line(),{var,line(),'Record'},{var,line(),'Data'}}]}]}},
						            {cons,new_line(),{var,line(),'EncodedRecs'},{var,line(),'Acc'}}]},
						          {clause,new_line(),
						           [{tuple,line(),
						             [{var,line(),'Pos'},
						              {atom,line(),repeated},
						              {var,line(),'Data'},
						              {var,line(),'Type'}]},
						            {var,line(),'Acc'}],
						           [],
						           [{match,new_line(),
						             {var,line(),'Encoded'},
						             {lc,line(),
						              {call,line(),
						               {remote,line(),{atom,line(),protobuffs},{atom,line(),encode}},
						               [{var,line(),'Pos'},{var,line(),'Item'},{var,line(),'Type'}]},
						              [{generate,line(),{var,line(),'Item'},{var,line(),'Data'}}]}},
						            {cons,new_line(),{var,line(),'Encoded'},{var,line(),'Acc'}}]},
						          {clause,new_line(),
						           [{tuple,line(),
						             [{var,line(),'Pos'},{var,line(),'_Tag'},{var,line(),'Data'},{var,line(),'Type'}]},
						            {var,line(),'Acc'}],
						           [],
						           [{'case',new_line(),
						             {call,line(),{atom,line(),atom_to_list},[{var,line(),'Type'}]},
						             [{clause,new_line(),
						               [{op,line(),'++',{string,line(),"int"},{var,line(),'_'}}],
						               [[{call,line(),{atom,line(),is_list},[{var,line(),'Data'}]}]],
						               [{cons,new_line(),
						                 {call,line(),
						                  {remote,line(),{atom,line(),protobuffs},{atom,line(),encode}},
						                  [{var,line(),'Pos'},
						                   {call,line(),{atom,line(),list_to_integer},[{var,line(),'Data'}]},
						                   {var,line(),'Type'}]},
						                 {var,line(),'Acc'}}]},
						              {clause,new_line(),
						               [{var,line(),'_'}],
						               [],
						               [{cons,new_line(),
						                 {call,line(),
						                  {remote,line(),{atom,line(),protobuffs},{atom,line(),encode}},
						                  [{var,line(),'Pos'},{var,line(),'Data'},{var,line(),'Type'}]},
						                 {var,line(),'Acc'}}]}]}]}]}},
						       {nil,new_line()},
						       {var,line(),'Data_Tuples'}]}]}]
				}]
	}).
	
value_to_tuple(Value) when is_atom(Value) -> {atom,line(),Value};
value_to_tuple(Value) when is_integer(Value) -> {integer,line(),Value};
value_to_tuple(Value) when is_binary(Value) -> {bytes,line(),Value};
value_to_tuple(Value) when is_list(Value) -> {string,line(),Value}.
		
write_decode(Basename, RecName, Fields) ->
	LRecName = string:to_lower(RecName),
	Decode_Function_Atom = list_to_atom("decode_" ++ LRecName),
	To_Record_Function_Atom = list_to_atom(LRecName ++ "_to_record"),

	append({function,new_line(),Decode_Function_Atom,1,
     [{clause,line(),
          [{var,line(),'Bytes'}],
          [[{call,line(),{atom,line(),is_binary},[{var,line(),'Bytes'}]}]],
          [{match,new_line(),
               {var,line(),'Data_Tuples'},
               {call,line(),
                   {remote,line(),{atom,line(),protobuffs},{atom,line(),decode_many}},
                   [{var,line(),'Bytes'}]}},
           {call,new_line(),{atom,line(),To_Record_Function_Atom},[{var,line(),'Data_Tuples'}]}]}]}),	

	append({function,new_line(),To_Record_Function_Atom,1,
	     [{clause,line(),
	          [{var,line(),'Data_Tuples'}],
	          [],
	          [{record,new_line(),list_to_atom(LRecName),
	               [begin
					 Func_Get_Value =
						case Tag of
							repeated -> get_values;
							_ -> get_value
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
	                        {atom,line(),unpack},
	                        [{call,line(),
	                             {atom,line(),Func_Get_Value},
	                             [{var,line(),'Data_Tuples'},
	                              {integer,line(),Pos},
	                              {Default_Type,line(),Default}]},
	                         {integer,line(),Pos},
	                         {string,line(),Type}]}}
					end || {Pos,Tag,Type,Name,_,Default} <- Fields]}]}]}),
					
	append({function,new_line(),get_value,3,
	     [{clause,line(),
	          [{var,line(),'Data_Tuples'},{var,line(),'Pos'},{var,line(),'Default'}],
	          [],
	          [{'case',new_line(),
	               {call,line(),
	                   {remote,line(),{atom,line(),proplists},{atom,line(),get_value}},
	                   [{var,line(),'Pos'},{var,line(),'Data_Tuples'}]},
	               [{clause,new_line(),[{atom,line(),undefined}],[],[{var,line(),'Default'}]},
	                {clause,new_line(),[{var,line(),'Value'}],[],[{var,line(),'Value'}]}]}]}]}),

	append({function,new_line(),get_values,3,
	     [{clause,line(),
	          [{var,line(),'Data_Tuples'},{var,line(),'Pos'},{var,line(),'Default'}],
	          [],
	          [{'case',new_line(),
	               {call,line(),
	                   {remote,line(),{atom,line(),proplists},{atom,line(),get_all_values}},
	                   [{var,line(),'Pos'},{var,line(),'Data_Tuples'}]},
	               [{clause,new_line(),[{nil,line()}],[],[{var,line(),'Default'}]},
	                {clause,new_line(),[{var,line(),'Values'}],[],[{var,line(),'Values'}]}]}]}]}),
	
	append({function,new_line(),unpack,3,
		 [begin
			{clause,line(),
		          [{var,line(),'Data'},{integer,line(),Pos},{string,line(),Type}],
		          [],
				  [case [Tag, Type] of
					
					[repeated, [C|_]] when C >= $A, C =< $Z -> %% repeated record
						{lc,new_line(),
						    {call,line(),
						        {atom,line(),apply},
						        [{atom,line(),list_to_atom(string:to_lower(Basename))},
						         {atom,line(),list_to_atom("decode_" ++ string:to_lower(Type))},
						         {cons,line(),{var,line(),'Item'},{nil,line()}}]},
						    [{generate,line(),{var,line(),'Item'},{var,line(),'Data'}}]};
						
					[repeated, _] -> %% repeated scalar
						{lc,new_line(),
						    {call,line(),
						        {remote,line(),{atom,line(),protobuffs},{atom,line(),decode}},
						        [{var,line(),'Item'},{atom,line(),list_to_atom(Type)}]},
						    [{generate,line(),{var,line(),'Item'},{var,line(),'Data'}}]};
						
					[_, [C|_]] when C >= $A, C =< $Z -> %% non-repeating record
						{call,new_line(),
						     {atom,line(),apply},
						     [{atom,line(),list_to_atom(string:to_lower(Basename))},
						      {atom,line(),list_to_atom("decode_" ++ string:to_lower(Type))},
						      {cons,line(),{var,line(),'Data'},{nil,line()}}]};
						
					[_, "string"] -> %% non-repeating scalar
						{call,new_line(),
						     {atom,line(),binary_to_list},
						     [{var,line(),'Data'}]}
						
				  end] }
		 end || {Pos,Tag,Type,_,_,_} <- Fields]}).

