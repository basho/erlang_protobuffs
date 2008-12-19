# README #

This module is a composite of other open source modules and original code to make interfacing with the Protocol Buffers protocol easy.

## Encode / Decode

Encoding is simple.

    1> erlang:iolist_to_binary(protobuffs:encode(1, 1, uint32)).
    <<8,1>>
    2> erlang:iolist_to_binary([protobuffs:encode(1, <<"Nick">>, string), protobuffs:encode(2, 25, uint32)]).
    <<10,4,78,105,99,107,16,25>>

Decoding is simple too.

    1> protobuffs:decode(<<8, 1>>, uint32).
    {{1, 1}, <<>>}
    2> protobuffs:decode(<<10,4,78,105,99,107,16,25>>, bytes).
    {{1, <<"Nick">>}, <<16,25>>}
    3> protobuffs:decode(<<16,25>>, bytes).
    {{2, 25}, <<>>}

Decoding a bunch of stuff is also simple.

    1> protobuffs:decode_many(<<10,4,78,105,99,107,16,25>>).
    [{1,<<"Nick">>},{2,25}]

## Using .proto Files

The main objective of this module is to allow developers to use .proto files easily. This module provides very basic functionality to do so.

Consider the `t/simple.proto` file.

    message Person {
    	required string name = 1;
    	required string address = 2;
    	required string phone_number = 3;
    	required int32 age = 4;
    }

From that file we can create an Erlang module that can encode and decode the Person message into records.

    1> protobuffs_compile:scan_file("simple.proto").
    ok
	2> c(simple_pb.erl).
	{ok,simple_pb}
	3> simple_pb:decode_person(<<10,4,78,105,99,107,18,13,77,111,117,110,116,97,105,110,32,86,105,101,119,26,17,43,49,32,40,48,48,...>>).
	{person,<<"Nick">>,<<"Mountain View">>, <<"+1 (000) 555-1234">>,25}
	4> simple_pb:encode_person({person,<<"Nick">>,<<"Mountain View">>, <<"+1 (000) 555-1234">>,25}).
	<<10,4,78,105,99,107,18,13,77,111,117,110,116,97,105,110,32,86,105,101,119,26,17,43,49,32,40,48,48,...>>

How cool is that?

## CAVEATS

Support for parsing proto files and creating code from it is volatile and should be considered alpha software at best. It currently only supports flat messages, simple types (ints, strings, etc) and will break on ENUM types and any sort of nesting. Please do not use this in production.

## CREDITS

Some of the protobuffs.erl module came from code written by Brian Buchanan.

Some of the protobuffs\_compile.erl module came from code written by Tim Fletcher.

The rest of it and it's test suite was written by Nick Gerakines.
