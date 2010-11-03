require 'rake'
require 'rake/clean'

# Configuration
START_MODULE = "erlang_protobuffs"
TEST_MODULE = "protobuffs"

# No Need to change
PWD = `pwd`.strip
INCLUDE = "include"
ERLC_FLAGS = "-I#{INCLUDE} +warn_unused_vars +warn_unused_import"

SRC = FileList['src/**/*.erl'] + FileList['test/**/*.erl']
YRL = FileList['src/**/*.yrl']
XRL = FileList['src/**/*.xrl']
OBJ = SRC.pathmap("%{src,ebin}X.beam") + YRL.pathmap("%X.beam") + XRL.pathmap("%X.beam")
CLEAN.include(['**/*.dump'])
CLOBBER.include(['**/*.beam','test/*.hrl','doc/*'] + YRL.pathmap("%X.erl") + XRL.pathmap("%X.erl"))

directory 'ebin'

rule ".beam" => ["%{ebin,src}X.erl"] do |t|
  if t.source[-14,14] == "pokemon_pb.erl"
      sh("erlc -pa ebin -W #{ERLC_FLAGS} +debug_info -o #{t.source.pathmap("%{src,ebin}d")} #{t.source}")
  else
      sh("erlc -pa ebin -W #{ERLC_FLAGS} -o #{t.source.pathmap("%{src,ebin}d")} #{t.source}")
  end
end

rule ".erl" => ".yrl" do |t|
  sh "erlc -o #{t.source.pathmap("%d")} #{t.source}"
end

rule ".erl" => ".xrl" do |t|
  sh "erlc -o #{t.source.pathmap("%d")} #{t.source}"
end

desc "Compile all"
task :compile => ['ebin'] + OBJ


desc "Open up a shell"
task :shell => [:compile] do
  sh("erl -sname #{START_MODULE} -pa #{PWD}/ebin -pa #{PWD}/test")
end

desc "Run Unit Tests"
task :test do
  sh("cd test; erl -noshell -pa #{PWD}/ebin -s eunit test #{TEST_MODULE} -s init stop")
end

desc "Generate Documentation"
task :doc do
    sh("cd doc && erl -noshell -run edoc files ../#{SRC.join(" ../")} -run init stop")
end

task :default => :compile
