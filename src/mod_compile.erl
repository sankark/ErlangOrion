%% @author Administrator
%% @doc @todo Add description to mod_compile.


-module(mod_compile).

%% ====================================================================
%% API functions
%% ====================================================================
-export([compile/1,load_module/1]).



%% ====================================================================
%% Internal functions
%% ====================================================================


%% main([File_Name]) ->
%%     Includes = case filelib:is_dir("../deps") of
%%                    false ->
%%                        [{i, "../include"}];
%%                    true ->
%%                        {ok, Deps} = file:list_dir("../deps"),
%%                        [{i, "../include"}| [{i,Di} || D <- Deps,
%%                               begin
%%                                   Di = "../deps/"++D++"/include",
%%                                   filelib:is_dir(Di)
%%                               end]]
%%                end,
%% 
%%     %% TODO: this should be cleaned up and some checking should be done before
%%     %% including ../../../ stuff
%%     IncludesAllApps = [{i, "../.."}, {i, "../../../deps"} | Includes],
%%     compile:file(File_Name, [warn_obsolete_guard, warn_unused_import,
%%                              warn_shadow_vars, warn_export_vars,
%%                              strong_validation, report | IncludesAllApps]).




%% handle_file(_Verb, ".erl", F) ->
%%     spawn(fun() ->
%%                   make:files([F], [load,
%%                                    {i, "include"},
%%                                    {i, "src/dbdrivers/postgresql/include"},
%%                                    {i, "deps/webzmachine/include"}, {outdir, "ebin"}])
%%           end),
%%     "Recompile " ++ F;
%% 
%% 
%% reload_module(M) ->
%% 	code:purge(M),
%% 	code:soft_purge(M),
%% 	{module, M} = code:load_file(M),
%% 	{ok, M}.



compile(FileName)->
	compile:file(FileName, [{outdir, filename:join([code:lib_dir(orion),"ebin"])},warn_obsolete_guard, warn_unused_import,
                              warn_shadow_vars, warn_export_vars,
                              return]).
load_module(M)->
	code:purge(M),
	code:soft_purge(M),
	{module, M} = code:load_file(M),
	{ok, M}.