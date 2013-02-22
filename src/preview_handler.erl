-module(preview_handler).
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


-record(state, {
	auth = undefined,
	collection = undefined :: boolean(),
	userid = undefined,
	bucket = undefined,
	data,
	path
}).

-define(BUCKET,<<"preview">>).

init(_, _, _) ->
	{upgrade, protocol, cowboy_rest}.

rest_init(Req, Opts) ->
	State = case lists:keyfind(auth, 1, Opts) of
		{auth, AuthOpts} -> #state{auth=AuthOpts};
		false -> #state{}
	end,
	{Name, Req2} = cowboy_req:path_info(Req),
	Bucket = proplists:get_value(bucket, Opts, ?BUCKET),
	{ok, Req2, State#state{collection=Name =:= [],path=Name,bucket=Bucket}}.

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
resource_exists(Req, State=#state{collection = false, path = Paths, bucket =Bucket})->
	[Template] = Paths,
	T2 = filename:basename(binary_to_list(Template),".dtl"),
	case orion_data_server:get_value(Bucket, list_to_binary(T2)) of
		{ok, Data} -> {true, Req, State#state{data=jsx:decode(Data)}};
		{error, notfound} -> {false, Req, State#state{data=[]}}
	end.

content_types_accepted(Req, State=#state{collection=false}) ->
	{[
		{<<"application/x-www-form-urlencoded;charset=UTF-8">>,
			template_from_form},
				{<<"text/plain;charset=UTF-8">>,
			template_from_form},
			{<<"application/xml">>,
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

template_to_html(Req, State=#state{path=Paths,data=Data}) ->
	[Template] = Paths,
	T2 = filename:basename(binary_to_list(Template),".dtl"),
	T =list_to_atom(T2 ++ "_dtl"),
	{ok, Body} = T:render(Data),
	send_page(Body, Req, State).

get_file_meta(Template) ->
				[{?KEY_NAME,Template}, {?KEY_DIRECTORY,false},
{?KEY_CHILDREN_LOCATION,list_to_binary("/orion/template/" ++ binary_to_list(Template) ++ "/?depth=1")},
{?KEY_LOCATION,list_to_binary("/orion/file/" ++ binary_to_list(Template) ++ "/")}].
			
send_page(Body,Req,State)->
	{Body, Req, State}.

template_to_json(Req, State) ->
	%% @todo
	Body = <<"{}">>,
	{Body, Req, State}.

template_from_json(Req, State=#state{path=Paths,bucket=Bucket}) ->
	%% @todo
	Path = get_path(Paths),
	{ok, [{Body,_}], _Req2} = cowboy_req:body_qs(Req),
	JsonReq = jsx:decode(Body),
	io:format("req $$$$$$$$$$$$$$~p~n",[JsonReq]),
	{_,Is_Dir} = lists:keyfind(<<"Directory">>, 1, JsonReq),
	{_,FileName} = lists:keyfind(<<"Name">>, 1, JsonReq),
	%%io:format("Is_Dir $$$$$$$$$$$$$$~p~n",[Is_Dir]),
	case Is_Dir of
		<<"true">>  -> file_util:create_file(Path++"/"++binary_to_list(FileName),true);
		<<"false">> -> file_util:create_file(Path++"/"++binary_to_list(FileName),false)
	end,
	RespBody = get_meta(Path++"/"++binary_to_list(FileName)),
	Req2 = cowboy_req:set_resp_body(RespBody,Req),	
	%%io:format("req $$$$$$$$$$$$$$~p~n",[JsonReq]),
	{true, Req2, State}.

get_path(Paths) -> filename:join([binary_to_list(P)||P <- Paths]).

%% @todo Later just send JSON from form directly.
template_from_form(Req, State=#state{path=Paths,bucket=Bucket}) ->
	{ok, [{Body,_}], _Req2} = cowboy_req:body_qs(Req),
	JsonReq = jsx:decode(Body),
	[{_,PrevData}] = JsonReq,
	Term = jsx:decode(PrevData),
	[Template] = Paths,
	T2 = filename:basename(binary_to_list(Template),".dtl"),
	orion_data_server:set_data(Bucket, list_to_binary(T2), <<"admin">>, PrevData, <<"admin">>),
	T=list_to_atom(T2++"_dtl"),
	io:format("PrevData ~p~n",[Term]),
	io:format("Json ~p~n",[JsonReq]),
	{ok, _RespBody} = T:render(Term),
	Resp = [{redirect,list_to_binary("/orion/preview/"++T2++".dtl")}],
	RespJson = jsx:encode(Resp),
	Req2 = cowboy_req:set_resp_body(RespJson,Req),	
	Req4 = cowboy_req:set_resp_header(<<"content-type">>,<<"application/json">>,Req2),
	%%io:format("req $$$$$$$$$$$$$$~p~n",[Body]),
			{true, Req4, State}.

set_data(Bucket,Values, Login) ->
    orion_data_server:set_data(Bucket, Login, Login, jsx:encode(Values), Login).

redirect(Req,Location,State) ->
    {ok, Reply} = cowboy_http_req:reply(
        302,
        [{<<"Location">>, Location}],
        <<"Redirecting with Header!">>,
        Req
    ),
    {ok, Reply, State}.

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
