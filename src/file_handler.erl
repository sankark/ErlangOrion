-module(file_handler).
-include_lib("../include/constants.hrl").
-export([init/3]).
-export([rest_init/2]).
-export([allowed_methods/2]).
-export([is_authorized/2]).
-export([content_types_provided/2]).
-export([charsets_provided/2]).
-export([resource_exists/2]).
-export([content_types_accepted/2]).
-export([is_conflict/2]).
-export([generate_etag/2]).
-export([collection_to_html/2]).
-export([collection_to_json/2]).
-export([template_to_html/2]).
-export([template_to_json/2]).
-export([template_from_json/2]).
-export([template_from_form/2]).
-export([delete_resource/2]).
-export([delete_completed/2]).
-export([create_file/2]).

-record(state, {
	auth = undefined,
	collection = undefined :: boolean(),
	userid = undefined,
	path
}).

init(_, _, _) ->
	{upgrade, protocol, cowboy_rest}.

rest_init(Req, Opts) ->
	State = case lists:keyfind(auth, 1, Opts) of
		{auth, AuthOpts} -> #state{auth=AuthOpts};
		false -> #state{}
	end,
	{Name, Req2} = cowboy_req:path_info(Req),
	{ok, Req2, State#state{collection=Name =:= [],path=Name}}.

allowed_methods(Req, State=#state{collection=true}) ->
	{[<<"GET">>], Req, State};
allowed_methods(Req, State=#state{collection=false}) ->
	{[<<"GET">>, <<"PUT">>, <<"DELETE">>], Req, State}.

is_authorized(Req, State=#state{auth=undefined}) ->
	{true, Req, State};
is_authorized(Req, State=#state{auth=AuthOpts}) ->
	case orion_auth:authenticate(Req, AuthOpts) of
		{false, Req2} ->
			{{false, orion_auth:methods(AuthOpts)}, Req2, State};
		{UserID, Req2} ->
			{true, Req2, State#state{userid=UserID}}
	end.

content_types_provided(Req, State=#state{collection=false}) ->
	{[
		{<<"text/html">>, template_to_html},
		{<<"application/json">>, template_to_json}
	], Req, State}.

%% Only allow UTF-8.
charsets_provided(Req, State) ->
	{[{<<"utf-8">>, 1000}, {<<"*">>, 0}], Req, State}.

resource_exists(Req, State=#state{collection=true}) ->
	{true, Req, State};
resource_exists(Req, State=#state{collection=false}) ->
	{true, Req, State}.

content_types_accepted(Req, State=#state{collection=false}) ->
	{[
		{<<"application/x-www-form-urlencoded;charset=UTF-8">>,
			template_from_form},
				{<<"text/plain;charset=UTF-8">>,
			template_from_form},
		{<<"application/json;charset=UTF-8">>, template_from_json}
	], Req, State}.

is_conflict(Req, State) ->
	case cowboy_req:qs_val(<<"new">>, Req) of
		{undefined, Req2} -> {false, Req2, State};
		{_, Req2} ->
			{Name, Req3} = cowboy_req:binding(name, Req2),
			case orion_templates_server:get_template(Name) of
				{ok, _} -> {true, Req3, State};
				{error, notfound} -> {false, Req3, State}
			end
	end.

%% Generate an ETag based on the compile time of this module.
generate_etag(Req, State=#state{collection=true}) ->
	{undefined, Req, State};
%% @todo This is cool but doesn't take into accounts templates
%% (both rendered and listed).
%%	{time, {Y, Mo, D, H, Mi, S}}
%%		= lists:keyfind(time, 1, code:module_info(compile)),
%%	ETag = base64:encode(<< Y:16, Mo:8, D:8, H:8, Mi:8, S:8 >>),
%%	{<< $", ETag/binary, $" >>, Req, State};
generate_etag(Req, State=#state{collection=false}) ->
	%% @todo ETag
	{undefined, Req, State}.

collection_to_html(Req, State) ->
	 Body =  <<"{
		  \"Workspaces\": [{
		    \"Id\": \"sankar\",
		    \"LastModified\": 1359697344754,
		    \"Location\": \"/orion/workspace/sankar\",
		    \"Name\": \"Orion Content\"
		  }]
		}">>,
	{Body, Req, State}.

collection_to_json(Req, State) ->
	%% @todo
	Body = <<"{}">>,
	{Body, Req, State}.

template_to_html(Req, State=#state{path=Paths}) ->
	{Parts, _Req3} = cowboy_req:qs_val(<<"parts">>,Req,false),
	io:format("Values ~p~n",[Req]),
	Path = filename:join([binary_to_list(P)||P<-Paths]),
	case Parts of
		false ->
				case file_util:get_files(Path) of
						{dir,Children}->Body = jsx:encode(Children),
										send_page(Body,Req,State);
							{file,Bin}->Ext = filename:extension(filename:join([code:lib_dir(orion),"priv",Path])),
										Req2 = cowboy_req:set_resp_header(<<"content-type">>,mime_type(classify_extension(Ext)),Req),
										send_page(Bin,Req2,State)
				end;
		<<"meta">> -> {ok,Children} = file_util:get_parts_meta(Path),
					  Body = jsx:encode(Children),
					  send_page(Body,Req,State)
	end.
			
send_page(Body,Req,State)->
	{Body, Req, State}.
classify_extension(".dtl") -> html;
classify_extension(".gif") -> gif;
classify_extension(".jpg") -> jpg;
classify_extension(".png") -> png;
classify_extension(".js")  -> js;
classify_extension(".css") -> css;
classify_extension(".ico")  -> ico;
classify_extension(_)      -> html.

mime_type(gif)     -> <<"image/gif">>;
mime_type(jpg)     -> <<"image/jpeg">>;
mime_type(png)     -> <<"image/png">>;
mime_type(ico)     -> <<"image/ico">>;
mime_type(css)     -> <<"text/css">>;
mime_type(special) -> <<"text/plain; charset=x-user-defined">>;
mime_type(json)    -> <<"application/json">>;
mime_type(swf)     -> <<"application/x-shockwave-flash">>;
mime_type(html)    -> <<"text/html">>;
mime_type(xul)     -> <<"application/vnd.mozilla.xul+xml">>;
mime_type(js)      -> <<"application/x-javascript">>;
mime_type(svg)     -> <<"image/svg+xml">>.

template_to_json(Req, State) ->
	%% @todo
	Body = <<"{}">>,
	{Body, Req, State}.

template_from_json(Req, State=#state{path=Paths}) ->
	%% @todo
	Path = get_path(Paths),
	Req2 = create_file(Req, Path),
	%%io:format("req $$$$$$$$$$$$$$~p~n",[JsonReq]),
	{true, Req2, State}.

create_file(Req, Path) ->
	   {ok, [{Body,_}], _Req2} = cowboy_req:body_qs(Req),
	   JsonReq = jsx:decode(Body),
	   io:format("req $$$$$$$$$$$$$$~p~n",[JsonReq]),
	   {_,Is_Dir} = lists:keyfind(<<"Directory">>, 1, JsonReq),
	   {_,FileName} = lists:keyfind(<<"Name">>, 1, JsonReq),
	   %%io:format("Is_Dir $$$$$$$$$$$$$$~p~n",[Is_Dir]),
		Path2 = case Path of
			""->"";
			Any -> Any ++ "/"
		end,
		  case Is_Dir of
		   <<"true">> ->file_util:create_file(Path2++ binary_to_list(FileName),true);
	    <<"false">> -> file_util:create_file(Path2++ binary_to_list(FileName),false)
	   end,
	   RespBody = get_meta(Path2 ++ binary_to_list(FileName)),
    cowboy_req:set_resp_body(RespBody,Req).

get_path(Paths) -> filename:join([binary_to_list(P)||P <- Paths]).

%% @todo Later just send JSON from form directly.
template_from_form(Req, State=#state{path=Paths}) ->
	Path = filename:join([binary_to_list(P)||P<-Paths]),
	{ok, Body, _Req3} = cowboy_req:body(Req),
	file_util:save_file(Path,Body),
	Resp_Body = get_meta(Path),
	Req2 = cowboy_req:set_resp_body(Resp_Body,Req),	
	%%io:format("req $$$$$$$$$$$$$$~p~n",[Body]),
			{true, Req2, State}.

get_meta(Path) ->
	   {ok,Children} = file_util:get_parts_meta(Path),
    jsx:encode(Children).

%% @todo template_from_json

delete_resource(Req, State=#state{path=Paths}) ->
	%% @todo
	Path = get_path(Paths),
	file_util:delete(Path),
	io:format("delete_resource ~p~n", [Req]),
	{true, Req, State}.

delete_completed(Req, State) ->
	%% @todo
	{true, Req, State}.
