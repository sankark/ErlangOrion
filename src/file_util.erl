%% Author: Administrator
%% Created: Feb 7, 2013
%% Description: TODO: Add description to file_server
-module(file_util).
-include_lib("../include/constants.hrl").
-include_lib("kernel/include/file.hrl").
%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([get_children/1]).
-export([get_files/1]).
-export([get_parts_meta/1]).
-export([save_file/2]).
-export([create_file/2]).
-export([delete/1]).

%%
%% API Functions
%%
-define(MILLI_SECONDS_PER_DAY, 86400000).

%%
%% Local Functions
%%

get_children(Path)->
	{ok, Files} = list_dir(Path),
	{ok, Dirs} = filter_dir(Files,Path),
    Files_Info = [{File,read_file_info(Path,File)}||File<-Dirs],
	Children = lists:foldl(fun({F,#file_info{type=Type,mtime=Mtime}},A) when Type =:= directory -> [[{?KEY_NAME,list_to_binary(F)},{?KEY_DIRECTORY,true},
				{?KEY_CHILDREN_LOCATION,list_to_binary("/orion/file/"++F++"/?depth=1")},
				{?KEY_LOCAL_TIMESTAMP,dat_time_to_millis(Mtime)},{?KEY_LOCATION,list_to_binary("/orion/file/"++F++"/")}]|A] end, [], Files_Info),
	{ok, Children}.


get_files(Path_Info)->
	Path = get_path(Path_Info),
	io:format("File Name ~p~n",[Path]),
	case filelib:is_dir(Path) of
	true ->
			get_meta(Path_Info, Path);
	false ->
	    read_file(Path)
	end.

get_path(Path_Info) ->
    filename:join([code:lib_dir(orion), "priv", Path_Info]).

read_file(Path) ->
		  Val = file:read_file(Path),
			 case Val of
			  {error,E} -> E;
	    {ok, Bin} -> {file, Bin}
    end.


get_parts_meta(Path_Info)->
	 {ok,Children}= get_meta3(filename:dirname(Path_Info),filename:dirname(get_path(Path_Info)),[filename:basename(Path_Info)]),
	 P2 = get_parents(Path_Info),
	 Ch1 = lists:nth(1, Children),
	 A = [P2|Ch1],
	 io:format("A value ~p~n",[A]),
	 {ok,A}.
get_meta(Path_Info, Path) ->
		  {ok, Children} = get_meta2(Path_Info, Path),
		  P2 = get_parents(Path_Info),
		  A = [P2|[{?KEY_CHILDREN,Children },{?KEY_NAME,list_to_binary(filename:basename(Path_Info))},
				   {?KEY_ID,<<"sankar">>},{?KEY_LOCATION,<<"/orion/workspace/sankar">>},{?KEY_CHILDREN_LOCATION,<<"sankar">>},{?KEY_DIRECTORY,<<"true">>}]],
	io:format("Children value ~p~n",[Children]),
	 io:format("Ch1 value ~p~n",[P2]),
		   io:format("Ch1 value ~p~n",[A]),
    	 {dir, A}.

get_meta2(Path_Info, Path) ->
    {ok, Files} = list_dir(Path),
	   get_meta3(Path_Info, Path, Files).

get_meta3(Path_Info, Path, Files) ->
	   Files_Info = [{File,read_file_info(Path,File)}||File <- Files],
				Children = lists:foldl(fun({F,#file_info{type=Type,mtime=Mtime}},A) ->
									IsDir = case Type of
									   directory -> true;
								    _ -> false
						           end,
									P3 = case Path_Info of
											 "" -> "";
										"/"++_P2 -> Path_Info ;
										      _-> "/"++Path_Info
									end,
									F3 = case F of
											  "" -> "";
										"/"++_F2 -> F ;
										      _-> "/"++F
									end,
						   [[{?KEY_NAME,list_to_binary(F)}, {?KEY_DIRECTORY,IsDir},
						     {?KEY_CHILDREN_LOCATION,list_to_binary("/orion/file" ++ P3 ++ F3 ++ "/?depth=1")},
	          {?KEY_LOCAL_TIMESTAMP,dat_time_to_millis(Mtime)}, {?KEY_LOCATION,list_to_binary("/orion/file" ++ P3++ F3)}]|A] end, [], Files_Info),
    {ok,Children}.

get_parents([Base|[]],Acc)->
	lists:reverse([[{name,Base},{location,[]}]|Acc]);
get_parents([Base|Rel],Acc)->
	get_parents(Rel,[[{name,Base},{location,filename:join(lists:reverse(Rel))}]|Acc]).
get_parents(Path_Info)->
	io:format("path info value ~p~n",[Path_Info]),
	 Split_Path = filename:split(filename:dirname(Path_Info)),
	 Parents = get_parents(lists:reverse(Split_Path),[]),
	 Parents_Array = [[{?KEY_NAME ,list_to_binary(Base)},
	 {?KEY_CHILDREN_LOCATION,list_to_binary("/orion/file" ++ add_slash(Loc)++add_slash(Base) ++"?depth=1")},
	 {?KEY_LOCATION,list_to_binary("/orion/file"++add_slash(Loc)++add_slash(Base))}]
	 ||[{name,Base},{location,Loc}]<-Parents],
	{?KEY_PARENTS,Parents_Array}.

add_slash(Loc) ->
case Loc of
"" -> "";
"/" ++ _P2 -> Loc;
_ -> "/" ++ Loc
    end.
	
dat_time_to_millis({{Y,M,D}, Time})->
	 ?MILLI_SECONDS_PER_DAY*calendar:date_to_gregorian_days(Y-1970,M,D) + calendar:time_to_seconds(Time)*1000.

list_dir(Dir) ->
    {ok, _Files} = file:list_dir(Dir).
filter_dir(Files,Root)->
	{ok,lists:filter(fun(F)-> filelib:is_dir(filename:join(Root, F)) end, Files)}.

read_file_info(Root,File)->
	Filename = filename:join(Root, File),
	Info = file_info(Filename),
	Info.

file_info(Filename) ->
    {ok, Info}=file:read_file_info(Filename),
    Info.

create_file(Path,IsDir)->
	Abs_Name = get_path(Path),
	case IsDir of
		true  ->
					file:make_dir(Abs_Name);
		false ->    {ok,Fid} = file:open(Abs_Name, write),
					file:write(Fid, <<"">>),
					file:close(Fid)
	end.
delete(Path)->
	Abs_Name = get_path(Path),
		io:format("Abs_Name $$$$$$$$$$$$$$~p~n",[Abs_Name]),
	case filelib:is_dir(Abs_Name) of
		true ->	file:del_dir(Abs_Name);
		false-> file:delete(Abs_Name)
	end.
	
save_file(Path,Content)->
	Abs_Name = get_path(Path),
	{ok,Fid} = file:open(Abs_Name, write),
	file:write(Fid, Content).
	
	
