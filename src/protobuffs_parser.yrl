Nonterminals
g_protobuffs g_members g_options g_option g_messages g_message g_enums g_enum g_elements g_element g_default.

Terminals ';' '=' '{' '}' '[' ']'
package option message enum var integer float string type requirement default.

Rootsymbol g_protobuffs.
Endsymbol '$end'.

g_protobuffs -> package var ';' g_members : [{package, safe_string('$2')}] ++ '$4'.
g_protobuffs -> g_members : '$1'.

g_members -> g_options g_messages : '$1' ++ '$2'.
g_members -> g_messages : '$1'. 

g_options -> g_option : ['$1'].
g_options -> g_option g_options : ['$1'|'$2'].
g_option -> option var '=' var ';' : {option, unwrap('$2'), unwrap('$4')}.
g_option -> option var '=' string ';' : {option, unwrap('$2'), unwrap('$4')}.

g_messages -> g_message : ['$1'].
g_messages -> g_message g_messages : ['$1'|'$2'].
g_message -> message var '{' g_elements '}' : {message, safe_string('$2'), '$4'}.

g_elements -> g_element : ['$1'].
g_elements -> g_element g_elements : ['$1'|'$2'].

g_element -> requirement type var '=' integer g_default ';' : {unwrap('$5'), unwrap('$1'), safe_string('$2'), safe_string('$3'), number, '$6'}.
g_element -> requirement var var '=' integer g_default ';' : {unwrap('$5'), unwrap('$1'), safe_string('$2'), safe_string('$3'), number, '$6'}.
g_element -> enum var '{' g_enums '}' : {enum, safe_string('$2'), '$4'}.
g_element -> g_message : '$1'.

g_enums -> g_enum : ['$1'].
g_enums -> g_enum g_enums : ['$1'|'$2'].

g_enum -> var '=' integer ';' : {enum, unwrap('$3'), safe_string('$1')}.

g_default -> '$empty' : none.
g_default -> '[' default '=' float ']' : unwrap('$4').
g_default -> '[' default '=' integer ']' : unwrap('$4').
g_default -> '[' default '=' var ']' : unwrap('$4').
g_default -> '[' default '=' string ']' : unwrap('$4').

Erlang code.
safe_string(A) -> make_safe(atom_to_list(unwrap(A))).

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

