%% @author Administrator
%% @doc @todo Add description to util.


-module(util).

%% ====================================================================
%% API functions
%% ====================================================================
-export([absolute_path/1]).



%% ====================================================================
%% Internal functions
%% ====================================================================

absolute_path(Path)->
	filename:join([code:lib_dir(orion), "priv",Path]).


	