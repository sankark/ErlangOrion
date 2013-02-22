-module(orion_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).

%% API.

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor.

init([]) ->
	Procs = [
	        {orion_config,
			{orion_config, start_link, []},
			permanent, 5000, worker, [orion_config]},
		{file_watcher,
			{file_watcher, start_link, []},
			permanent, 5000, worker, [file_watcher]}	
	],
	{ok, {{one_for_one, 10, 10}, Procs}}.
