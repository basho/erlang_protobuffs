Nonterminals
g_protobuffs g_members g_members_noopts g_member g_options g_option 
g_message g_service g_rpcs g_rpc g_enums g_enum g_elements 
g_element g_default g_pack g_var.

Terminals ';' '=' '{' '}' '[' ']' '(' ')'
package option message enum var integer float string type requirement default pack to extensions max service rpc returns.

Rootsymbol g_protobuffs.
Endsymbol '$end'.

g_protobuffs -> package g_var ';' g_members : [{package, safe_string('$2')}] ++ '$4'.
g_protobuffs -> g_members : '$1'.

g_members -> g_options g_members_noopts : '$1' ++ '$2'.
g_members -> g_members_noopts : '$1'.

g_members_noopts -> g_member : ['$1'].
g_members_noopts -> g_member g_members_noopts : ['$1'|'$2'].

g_member -> g_message : '$1'.
g_member -> enum g_var '{' g_enums '}' : {enum, safe_string('$2'), '$4'}.
g_member -> g_service : '$1'.

g_service -> service g_var '{' g_rpcs '}' : {service, safe_string('$2'), '$4'}.

g_rpcs -> g_rpc : ['$1'].
g_rpcs -> g_rpc g_rpcs : ['$1'|'$2'].

g_rpc -> rpc g_var '(' g_var ')' returns '(' g_var ')' ';' : {rpc, safe_string('$2'), safe_string('$4'), safe_string('$8')}.

g_options -> g_option : ['$1'].
g_options -> g_option g_options : ['$1'|'$2'].
g_option -> option g_var '=' g_var ';' : {option, '$2', '$4'}.
g_option -> option g_var '=' string ';' : {option, '$2', '$4'}.

g_message -> message g_var '{' g_elements '}' : {message, safe_string('$2'), '$4'}.

g_elements -> g_element : ['$1'].
g_elements -> g_element g_elements : ['$1'|'$2'].

g_element -> requirement type g_var '=' integer g_default ';' : {unwrap('$5'), unwrap('$1'), safe_string(unwrap('$2')), safe_string('$3'), number, '$6'}.
g_element -> requirement g_var g_var '=' integer g_default ';' : {unwrap('$5'), unwrap('$1'), safe_string('$2'), safe_string('$3'), number, '$6'}.

g_element -> requirement type g_var '=' integer g_pack ';' : {unwrap('$5'), pack_repeated(unwrap('$1'),'$6'), safe_string(unwrap('$2')), safe_string('$3'), number, none}.
g_element -> requirement g_var g_var '=' integer g_pack ';' : {unwrap('$5'), pack_repeated('$1','$6'), safe_string('$2'), safe_string('$3'), number, none}.  
g_element -> extensions integer to integer ';' : {unwrap('$1'), unwrap('$2'), unwrap('$4')}.
g_element -> extensions integer to max ';' : {unwrap('$1'), unwrap('$2'), unwrap('$4')}.

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
g_var -> extensions : unwrap('$1').
g_var -> to : to.
g_var -> max : max.
g_var -> service : service.
g_var -> rpc : rpc.
g_var -> returns : returns.


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

