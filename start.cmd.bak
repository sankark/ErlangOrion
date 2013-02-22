set erl=erl
rem set erl="C:\Program Files\erl5.7.4\bin\erl.exe"

set ebin=./ebin ./deps/uuid/ebin ./deps/cowboy/ebin ./deps/bootstrap ./jquery ./deps/dh_date/ebin ./deps/edown/ebin ./deps/erlydtl/ebin ./deps/gen_leader/ebin ./deps/gproc/ebin ./deps/jsx/ebin ./deps/lager/ebin ./deps/meck/ebin ./deps/mimetypes/ebin ./deps/protobuffs/ebin ./deps/ranch/ebin ./deps/riak_pb/ebin ./deps/riakc/ebin 
%erl%  +P 10000000 -pa %ebin% -env ERL_MAX_ETS_TABLES 20000 -sname farwest -setcookie cookie -boot start_sasl -sasl errlog_type error -s farwest
