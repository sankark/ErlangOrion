-module(orion_app).
-behaviour(application).

-export([start/0]).
-export([start/2]).
-export([stop/1]).

start() ->
	start([],[]).
start(_, _) ->
	set_path(),
	Port = int_env(http_port, 8085),
	SSLPort = int_env(https_port, 8443),
	Certfile = path_env(https_cert),
	CACertfile = path_env(https_cacert),
	{ok, Routes} = file:consult(path_env(routes_file)),
	%% HTTP.
	io:format("routes ~p",[Routes]),
	{ok, _} = cowboy:start_http(orion_http, 100,
		[{ip,{127,0,0,1}},{port, Port}], [{dispatch, Routes}]
	),
	%%lager:info("orion listening on port ~p~n", [Port]),
	{ok, _} = cowboy:start_https(orion_https, 100,
		[{port, SSLPort}, {certfile, Certfile},
			{cacertfile, CACertfile}, {verify, verify_peer}],
		[{dispatch, Routes}]
	),
	%%lager:info("orion securely listening on port ~p~n", [SSLPort]),
	orion_sup:start_link().

stop(_) ->
	ok.

%% Internal.

int_env(Key, Default) ->
	case application:get_env(orion, Key) of
		{ok, Value} when is_integer(Value) ->
			Value;
		undefined ->
			Default
	end.

path_env(Key) ->
	case application:get_env(orion, Key) of
		{ok, {priv_dir, App, Path}} ->
			code:priv_dir(App) ++ "/" ++ Path;
		{ok, Path} ->
			Path
	end.
set_path() ->
	P = code:all_loaded(),
	Path = filename:dirname(filename:dirname(proplists:get_value(?MODULE, P))),
	code:add_pathz(Path),
	application:set_env(orion, lib_dir, Path),
	application:set_env(orion, https_cert, Path ++ "/priv/cert/sample.crt"),
	application:set_env(orion, https_cacert,Path ++ "/priv/cert/sample.cert"),
	application:set_env(orion, config_file,Path ++ "/priv/config"),
	application:set_env(orion, routes_file,Path ++ "/priv/dispatch").