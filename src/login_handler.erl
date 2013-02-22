-module(login_handler).

-export([init/3]).
-export([rest_init/2]).
-export([is_authorized/2]).
-export([content_types_provided/2]).
-export([charsets_provided/2]).
-export([to_html/2]).
-export([to_json/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([process_post/2]).
-export([template_from_json/2]).
-export([template_from_form/2]).

init(_, _, _) ->
	{upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
	
	{ok, Req, []}.

is_authorized(Req, State) ->
	{true, Req, State}.


allowed_methods(Req, State) ->
	{[<<"GET">>, <<"PUT">>,<<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
	{[
		{<<"text/html">>, to_html},
		{<<"application/json">>, to_json}
	], Req, State}.

%% Only allow UTF-8.
charsets_provided(Req, State) ->
	{[{<<"utf-8">>, 1000}, {<<"*">>, 0}], Req, State}.

content_types_accepted(Req, State) ->
	{[
		{<<"application/x-www-form-urlencoded;charset=UTF-8">>,
			template_from_form},
		{<<"text/plain;charset=UTF-8">>,
			template_from_form},
		{<<"application/xml">>,
			template_from_form},
		{<<"application/json">>, template_from_json}
	], Req, State}.



to_html(Req, State) ->
	
      Body = <<"success">>,
	{Body, Req, State}.

to_json(Req, State) ->
	%% @todo
	Body = <<"{success}">>,
	{Body, Req, State}.

%% Internal.

template_from_json(Req, State) ->
	%% @todo
	Body = <<"{success}">>,
	{Body, Req, State}.

%% @todo Later just send JSON from form directly.
template_from_form(Req, State) ->
	Body = <<"{\"lastlogintimestamp\":1359725142013,\"uid\":\"E\",\"Plugins\": [],\"properties\": {},\"hasPassword\": true,\"email\": \"gddds@dgvrd.com\",\"emailConfirmed\": true,\"login\":\"sxdfgdfg\",\"Location\":\"/users/E\",\"Name\":\"sxdfgdfg\",\"CanAddUsers\":true,\"RegistrationURI\":\"http://test/f\",\"sankar\":\"true\",\"ForceEmail\":false,\"emailConfigured\":\"true\"}">>,
	Req2 = cowboy_req:set_resp_body(Body,Req),		
	{true, Req2, State}.
	
process_post(Req, State) ->
	Req2 = cowboy_req:set_resp_body(<<"{\"lastlogintimestamp\":1359725142013,\"uid\":\"E\",\"Plugins\": [],\"properties\": {},\"hasPassword\": true,\"email\": \"gddds@dgvrd.com\",\"emailConfirmed\": true,\"login\":\"sxdfgdfg\",\"Location\":\"/users/E\",\"Name\":\"sxdfgdfg\",\"CanAddUsers\":true,\"RegistrationURI\":\"http://test/f\",\"sankar\":\"true\",\"ForceEmail\":false,\"emailConfigured\":\"true\"}">>, Req),		
	Req4 = cowboy_req:set_resp_header(<<"content-type">>,<<"application/json">>,Req2),
	{true, Req4, State}.