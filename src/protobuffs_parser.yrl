Nonterminals
g_protobuffs g_members g_options g_option g_messages g_message g_enums g_enum g_elements g_element g_default g_pack g_var.

Terminals ';' '=' '{' '}' '[' ']'
package option message enum var integer float string type requirement default pack.

Rootsymbol g_protobuffs.
Endsymbol '$end'.

g_protobuffs -> package g_var ';' g_members : [{package, safe_string('$2')}] ++ '$4'.
g_protobuffs -> g_members : '$1'.

g_members -> g_options g_messages : '$1' ++ '$2'.
g_members -> g_messages : '$1'. 

g_options -> g_option : ['$1'].
g_options -> g_option g_options : ['$1'|'$2'].
g_option -> option g_var '=' g_var ';' : {option, '$2', '$4'}.
g_option -> option g_var '=' string ';' : {option, '$2', '$4'}.

g_messages -> g_message : ['$1'].
g_messages -> g_message g_messages : ['$1'|'$2'].
g_message -> message g_var '{' g_elements '}' : {message, safe_string('$2'), '$4'}.

g_elements -> g_element : ['$1'].
g_elements -> g_element g_elements : ['$1'|'$2'].

g_element -> requirement type g_var '=' integer g_default ';' : {unwrap('$5'), unwrap('$1'), safe_string(unwrap('$2')), safe_string('$3'), number, '$6'}.
g_element -> requirement g_var g_var '=' integer g_default ';' : {unwrap('$5'), unwrap('$1'), safe_string('$2'), safe_string('$3'), number, '$6'}.

g_element -> requirement type g_var '=' integer g_pack ';' : {unwrap('$5'), pack_repeated(unwrap('$1'),'$6'), safe_string(unwrap('$2')), safe_string('$3'), number, none}.
g_element -> requirement g_var g_var '=' integer g_pack ';' : {unwrap('$5'), pack_repeated('$1','$6'), safe_string('$2'), safe_string('$3'), number, none}.

g_element -> enum g_var '{' g_enums '}' : {enum, safe_string('$2'), '$4'}.
g_element -> g_message : '$1'.

g_enums -> g_enum : ['$1'].
g_enums -> g_enum g_enums : ['$1'|'$2'].

g_enum -> g_var '=' integer ';' : {enum, unwrap('$3'), safe_string('$1')}.

g_default -> '$empty' : none.
g_default -> '[' default '=' float ']' : unwrap('$4').
g_default -> '[' default '=' integer ']' : unwrap('$4').
g_default -> '[' default '=' g_var ']' : '$4'.
g_default -> '[' default '=' string ']' : unwrap('$4').

g_pack -> '[' pack '=' g_var ']' : '$4'.

g_var -> var : unwrap('$1').
g_var -> package : package.
g_var -> option : option.
g_var -> message : message.
g_var -> enum : enum.
g_var -> integer : integer.
g_var -> float : flaot.
g_var -> string : unwrap('$1').
g_var -> default : default.
g_var -> pack : pack.
g_var -> type : unwrap('$1').
g_var -> requirement : unwrap('$1').

Erlang code.
safe_string(A) -> make_safe(atom_to_list(A)).

reserved_words() ->
  ["after", "and", "andalso", "band", "begin", "bnot", "bor", "bsl", "bsr", "bxor", "case", "catch", "cond", "div", "end", "fun",
   "if", "let", "not", "of", "or", "orelse", "query", "receive", "rem", "try", "when", "xor"].

make_safe(String) ->
  case lists:any(fun(Elem) -> string:equal(String,Elem) end, reserved_words()) of 
    true -> "pb_"++String;
    false -> String
  end.

unwrap({_,_,V}) -> V;
unwrap({V,_}) -> V.

pack_repeated(repeated,true) ->
  repeated_packed;
pack_repeated(repeated,_) ->
  repeated.

