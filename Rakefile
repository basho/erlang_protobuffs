require 'rake'
require 'rake/clean'

# Configuration
START_MODULE = "erlang_protobuffs"
TEST_MODULE = "test_erlang_protobuffs"

# No Need to change
INCLUDE = "include"
ERLC_FLAGS = "-I#{INCLUDE} +warn_unused_vars +warn_unused_import"

SRC = FileList['src/**/*.erl']
OBJ = SRC.pathmap("%{src,ebin}X.beam")
CLEAN.include(['**/*.dump'])
CLOBBER.include(['**/*.beam'])

directory 'ebin'

rule ".beam" => ["%{ebin,src}X.erl"] do |t|
  if t.source[-14,14] == "pokemon_pb.erl"
      sh "erlc -pa ebin -W #{ERLC_FLAGS} +debug_info -o ebin #{t.source}"
  else
    sh "erlc -pa ebin -W #{ERLC_FLAGS} -o ebin #{t.source}"
  end
end

desc "Compile all"
task :compile => ['ebin'] + OBJ


desc "Open up a shell"
task :shell => [:compile] do
  sh("erl -sname#{START_MODULE} -pa #{PWD}/ebin")
end

desc "Run Unit Tests"
task :test do
  sh("erl -noshell -s #{TEST_MODULE} test -s init stop")
end

desc "Generate Documentation"
task :doc do
  sh("cd doc && erl -noshell edoc files ../#{SRC.join(" ../")} -run init stop")
end

task :default => :compile
