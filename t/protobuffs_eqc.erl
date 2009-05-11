%%% File    : protobuffs_eqc.erl
%%% Author  :  <thomas@QUVIQ-THOMAS>
%%% Description : QuickCheck specification used in class for
%%%               protobuffs-0.1
%%% Created : 27 Apr 2009 by  <thomas@QUVIQ-THOMAS>
-module(protobuffs_eqc).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

-define(NotYetImplemented(Cond,Prop),?IMPLIES(not (Cond),Prop)).

%% eqc_gen:sample(protobuffs_eqc:field_num()).
%% eqc:quickcheck(protobuffs_eqc:prop_encode_decode2()).

%%%%%%%%%%%%%%% Properties %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prop_encode_decode2() ->
    ?FORALL({FieldNum,Data,Type},fault_rate(5,10,protobuff_data()),
		 case catch protobuffs:encode(FieldNum,Data,Type) of
		     {'EXIT', _} ->
			     not in_range(Data,Type);
		     Bin ->
			     {{N, RData}, <<>>} = protobuffs:decode(list_to_binary(Bin), Type),
    			 in_range(Data,Type) andalso
    			     FieldNum=:=N andalso
    			     (Data==RData orelse foreign_type(Type,Data,RData))  
	     end).

prop_encode_decode() ->
    ?FORALL({FieldNum,Data,Type},protobuff_data(),
	    collect(Type,
    	    begin
        		{{N, RData}, <<>>} = protobuffs:decode(list_to_binary(protobuffs:encode(FieldNum, Data, Type)), Type),
        		FieldNum=:=N andalso 
        		(Data==RData orelse foreign_type(Type,Data,RData))  
    	    end
	    )).

foreign_type(bool,false,0) ->
    true;
foreign_type(bool,true,1) ->
    true;
foreign_type(_,_,_) ->
    false.

prop_varint() ->
    ?FORALL(Base,oneof([32,64]),
    ?FORALL(I,int(Base),
       ?NotYetImplemented(I<0,
            begin
		{Bits,Data} = decompose(protobuffs:encode_varint(I)),
		right_bits(Bits) andalso 
		    concatenate(Data) == I
	    end))).

%% Bits are in reverse order: First bit should be zero, rest should be 1 
right_bits([0|Rest]) ->
    lists:all(fun(B) -> B==1 end,Rest).

%%%%%%%%%%%%%%% Data generators %%%%%%%%%%%%%%%%%%%%%

protobuff_data() ->
    fault({field_num(), int(80), oneof([int32,uint32,int64,uint64,sint32,sint64])},
          oneof([{field_num(), int(32),int32},
	             {field_num(), uint(32),uint32},
	             {field_num(), int(64),int64},
	             {field_num(), uint(64),uint64},
	             {field_num(), bool(),bool},
                 {field_num(), sint(32),sint32},
		         {field_num(), sint(64),sint64}])).

field_num() ->
    ?SUCHTHAT(N,nat(),N>0).

int(Base) ->
    ?LET(I,uint(Base),
	 begin 
	     << N:Base/signed >> = <<I:Base>>, N 
	 end).

uint(Base) ->
    oneof([ choose(0,exp(B)) || B<-lists:seq(1,Base)]).

sint(Base) ->
    int(Base).

exp(1) ->
    2;
exp(N) ->
    2*exp(N-1).

%uint(32) ->
%    oneof([choose(0,16#ff),choose(0,16#ffff),choose(0,16#ffffff),
%	   choose(0,16#ffffffff)]);
%uint(64) ->
%    oneof([uint(32),choose(0,16#ffffffffffffffff)]);
%uint(80) ->
%    oneof([uint(32),uint(64),choose(0,16#ffffffffffffffffffff)]).

%%%%%%%%%%%%%%%%% Helper functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%

decompose(<<Bit:1,Data:7>>) ->
    {[Bit],<<Data:7>>};
decompose(<<Bit:1,Data:7,Rest/binary>>) ->
    {Bs,Ds} = decompose(Rest),
    {Bs++[Bit],<<Ds/bitstring,Data:7>>}.

concatenate(Bin) ->
    S = bit_size(Bin),
    << N:S >> = Bin,
    N.

in_range(Int,int32) ->
    fitbits(Int,32);
in_range(Int,sint32) ->
    fitbits(abs(Int),31);
in_range(Int,uint32) ->
    fitbits(Int,32);
in_range(Int,int64) ->
    fitbits(Int,64);
in_range(Int,sint64) ->
    fitbits(abs(Int),63);
in_range(Int,uint64) ->
    fitbits(Int,64);
in_range(false,bool) ->
    true;
in_range(true,bool) ->
    true.

fitbits(Int,Bits) ->
    RestBits = 80-Bits,
    << NoFit:RestBits, _:Bits >> = <<Int:80>>,
    NoFit == 0.
