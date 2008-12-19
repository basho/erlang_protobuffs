-module(protobuffs_t_001).
-export([start/0]).

start() ->
    etap:plan(12),
    etap_can:loaded_ok(protobuffs, "module 'protobuffs' loaded"),
    etap_can:can_ok(protobuffs, encode),
    etap_can:can_ok(protobuffs, encode, 3),
    etap_can:can_ok(protobuffs, decode),
    etap_can:can_ok(protobuffs, decode, 2),
    etap_can:loaded_ok(protobuffs_compile, "module 'protobuffs_compile' loaded"),
    etap_can:can_ok(protobuffs_compile, scan_file),
    etap_can:can_ok(protobuffs_compile, scan_file, 1),
    etap_can:can_ok(protobuffs_compile, scan),
    etap_can:can_ok(protobuffs_compile, scan, 1),
    etap_can:can_ok(protobuffs_compile, parse),
    etap_can:can_ok(protobuffs_compile, parse, 1),
    etap:end_tests().
