%% @author Administrator
%% @doc @todo Add description to mod_dispatch.


-module(mod_dispatch).

%% ====================================================================
%% API functions
%% ====================================================================
-export([update/1]).



%% ====================================================================
%% Internal functions
%% ====================================================================

update(_Args)->
	io:format("updating"),
	Dispatch = orion_config:get(dispatch_file),
	{ok, Routes} = file:consult(Dispatch),
	ranch:set_protocol_options(orion_http,[{dispatch, Routes}]).
