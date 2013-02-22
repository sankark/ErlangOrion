-module(compile_handler).
-export([init/3]).
-export([rest_init/2]).
-export([allowed_methods/2]).
-export([is_authorized/2]).
-export([charsets_provided/2]).
-export([content_types_accepted/2]).
-export([template_from_json/2]).
-export([content_types_provided/2]).


-record(state, {
	auth = undefined,
	userid = undefined
}).

init(_, _, _) ->
	{upgrade, protocol, cowboy_rest}.

rest_init(Req, Opts) ->
	State = case lists:keyfind(auth, 1, Opts) of
		{auth, AuthOpts} -> #state{auth=AuthOpts};
		false -> #state{}
	end,
	{ok, Req, State#state{}}.


allowed_methods(Req, State) ->
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


%% Only allow UTF-8.
charsets_provided(Req, State) ->
	{[{<<"utf-8">>, 1000}, {<<"*">>, 0}], Req, State}.

content_types_provided(Req, State) ->
	{[
		{<<"text/plain">>, template_to_html},
		{<<"application/json">>, template_to_json}
	], Req, State}.

content_types_accepted(Req, State) ->
	{[{<<"application/json;charset=UTF-8">>, template_from_json}], Req, State}.


template_from_json(Req, State) ->
	%% @todo
	{ok, [{Body,_}], _Req2} = cowboy_req:body_qs(Req),
	JsonReq = jsx:decode(Body),
	Location = binary_to_list(proplists:get_value(<<"Location">>, JsonReq)),
	Loc2 = remove_url_string(Location),
	Absolute = util:absolute_path(Loc2),
	Resp = mod_compile:compile(Absolute),
	RespBody = convert([Resp]),
	case Resp of
		{ok,Mod,_}->mod_compile:load_module(Mod);
				_ -> Resp
	end,
	%%Req2 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req),
	Req3 = cowboy_req:set_resp_body(RespBody,Req),	
	{true, Req3, State}.

convert(Message) ->
	Rep2 = io_lib:format("\"~p\"",Message),
	Rep3 = lists:flatten(Rep2),
    list_to_binary(Rep3).

remove_url_string(Loc)->
	case Loc of
		"/orion/file/"++ _Dir -> _Dir;
		_ -> Loc
	end.
