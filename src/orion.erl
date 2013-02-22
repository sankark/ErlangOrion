-module(orion).

%% API.
-export([start/0]).

%% API.

-spec start() -> ok.
start() ->
	ok = application:start(compiler),
	ok = application:start(syntax_tools),
	ok = application:start(crypto),
	ok = application:start(public_key),
	ok = application:start(ssl),
	ok = application:start(lager),
	ok = application:start(gproc),
	ok = application:start(ranch),
	ok = application:start(cowboy),
	application:start(orion).

