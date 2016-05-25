About the benchmarks
--------------------

The benchmark setup is very similar to the benchmarks found in the
protobuf's subversion repository.

Deviations from the benchmarks in protobuf's subversion repository:

* The google_size.proto and the google_speed.proto have been
  fused into msg.proto since gpb doesn't know how to optimize
  neither for speed nor for code size.
* The "repeated group" construction has been changed into a
  repeated (sub) message, since groups are not supported by gpb.
  The google_message2.dat file has been updated accordingly.

Running a benchmark
-------------------

1. Build `erlang_protobuffs` in parent dir:
   $ make

2. Build the benchmarking code (in this directory):
   $ make

3. Run the benchmarks:
   $ make benchmarks

4. Wait! Each test runs for around 30--35 seconds, and there are 2 tests
   per msg/data combination.
