{application, protobuffs,
 [
    {description, "Google protobuffs implementation for Erlang."},
    {vsn, "3"},
    {modules, [
		pokemon_pb,
		protobuffs,
		protobuffs_compile,
		protobuffs_parser
    ]},
    {registered, []},
    {applications, [
                    kernel,
                    stdlib,
                    sasl,
                    crypto
                   ]}
]}.
