-module(orion_client_handler).

-export([init/3, terminate/2, handle/2]).

-import(ezwebframe_mochijson2, [encode/1, decode/1]).



init(_, Req, E0) ->   
    Resource = path(Req),
    %io:format("init Resource =~p Env=~p~n",[Resource, E0]),
    case Resource of
	["/", "websocket",_] ->
	    %% The upgrade return value will cause cowboy
	    %% to call this module at the entry point
	    %% websocket_init
	    io:format("upgrading:~n"),
	    {upgrade, protocol, cowboy_websocket};
	_ ->
	    {ok, Req, E0}
    end.

terminate(_, _) ->  
    ok.

%% env has only one parameter - reserved for future expansion

handle(Req, Env) ->
 %io:format("insid handle ~p",[Req]),
  %io:format("path_info ^^^^^ handle ~p",[cowboy_req:path(Req)]),
 
    Resource = filename:join(path(Req)),
    %io:format("ezwebframe:handle ~p~n",[Resource]),
    Dir = dir(2, code:which(orion)) ++ "/priv",
    Res1 = Dir ++ Resource,
    %io:format("mapped to:~p~n",[Res1]),
    serve_file(Res1, Req, Env).

dir(0, F) -> F;
dir(K, F) -> dir(K-1, filename:dirname(F)).

serve_file(File, Req, Env) ->
    case filelib:is_dir(File) of
	true ->
	    list_dir(File, Req, Env);
	false ->
	    serve_abs_file(File, Req, Env)
    end.

serve_abs_file(File, Req, Env) ->
    %io:format("serve_abs:~p~n",[File]),
    Val = file:read_file(File),
    case Val of 
	{error, _} ->
	    io:format("*** no page called ~p~n",[File]),
	    reply_html(pre({no_page_called,File}), Req, Env);
	{ok, Bin} ->
	    Ext = filename:extension(File),
	    {ok, Req1} = send_page(classify_extension(Ext), Bin, Req),
	    {ok, Req1, Env}
    end.

list_dir(Root, Req, Env) ->
    %io:format("List dir:~p~n",[Root]),
    {ok, Files} = file:list_dir(Root),
    Files1 = [add_slash(I, Root) || I <- Files],
    L1 = [["<li><a href='",I,"'>",I,"</a></li>\n"] || I <- lists:sort(Files1)],
    reply_html(["<h1> Directory ",Root, "</h1>\n",
		"<ul>\n",L1,"</ul>\n"], Req, Env).

add_slash(I, Root) ->
    %io:format("Add slash:~p ~p~n",[I,Root]),
    Full = filename:join(Root, I),
    case filelib:is_dir(Full) of
	true ->
	    I ++ "/";
	false ->
	    I
    end.

send_page(Type, Data, Req) ->
    cowboy_req:reply(200, [{<<"Content-Type">>,
			    list_to_binary(mime_type(Type))}],
		     Data, Req).

classify_extension(".gif") -> gif;
classify_extension(".jpg") -> jpg;
classify_extension(".png") -> png;
classify_extension(".js")  -> js;
classify_extension(".css") -> css;
classify_extension(_)      -> html.

mime_type(gif)     -> "image/gif";
mime_type(jpg)     -> "image/jpeg";
mime_type(png)     -> "image/png";
mime_type(css)     -> "text/css";
mime_type(special) -> "text/plain; charset=x-user-defined";
mime_type(json)    -> "application/json";
mime_type(swf)     -> "application/x-shockwave-flash";
mime_type(html)    -> "text/html";
mime_type(xul)     -> "application/vnd.mozilla.xul+xml";
mime_type(js)      -> "application/x-javascript";
mime_type(svg)     -> "image/svg+xml".


pre(X) ->
    ["<pre>\n",quote(lists:flatten(io_lib:format("~p",[X]))), "</pre>"].

quote("<" ++ T) -> "&lt;" ++ quote(T);
quote("&" ++ T) -> "&amp;" ++ quote(T);
quote([H|T])    -> [H|quote(T)];
quote([])       -> [].

path(Req) ->
    {Path,_} = cowboy_req:path(Req),
    P = filename:split(binary_to_list(Path)),
    %io:format("Path=~p~n",[P]),
    P.

%% args(Req) ->
%%     {Args, _} = cowboy_req:qs_vals(Req),
%%     Args.

%% reply_type(Type, Data, Req, Env) ->
%%     {ok, Req1} = send_page(Type, Data, Req),
%%     {ok, Req1, Env}.

reply_html(Obj, Req, Env) ->
    {ok, Req1} = send_page(html, Obj, Req),
    {ok, Req1, Env}.
