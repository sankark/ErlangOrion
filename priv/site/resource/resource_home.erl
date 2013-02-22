%%%-------------------------------------------------------------------
%%% Author  : Administrator
%%% Description : 
%%%
%%% Created : Feb 22, 2013
%%%-------------------------------------------------------------------

%% @author Administrator
%% @doc @todo Add description to hello.


-module(resource_home).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------


-export([init/3]).
-export([rest_init/2]).
-export([is_authorized/2]).
-export([content_types_provided/2]).
-export([charsets_provided/2]).
-export([to_html/2]).
-export([to_json/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([template_from_json/2]).
-export([template_from_form/2]).
-export([resource_exists/2]).
-export([delete_resource/2]).
-export([delete_completed/2]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(state,{}).
%%====================================================================
%% External functions
%%====================================================================


init(_, _, _) ->
	{upgrade, protocol, cowboy_rest}.

rest_init(Req, Opts) ->
	%% @todo
	State =  #state{},
	{ok, Req, State}.

is_authorized(Req, State) ->
	{true, Req, State}.


allowed_methods(Req, State) ->
	{[<<"GET">>, <<"PUT">>,<<"DELETE">>], Req, State}.


content_types_provided(Req, State) ->
	{[
		{<<"text/html">>, to_html},
		{<<"application/json">>, to_json}
	], Req, State}.

charsets_provided(Req, State) ->
	{[{<<"utf-8">>, 1000}, {<<"*">>, 0}], Req, State}.

content_types_accepted(Req, State) ->
	{[
		{<<"text/html; charset=UTF-8">>,
			template_from_form},
		{<<"application/json; charset=UTF-8">>, template_from_json}
	], Req, State}.


resource_exists(Req, State)->
	{true, Req, State}.


to_html(Req, State)  ->
	{ok,Body} = test_dtl:render([]),
	{Body, Req, State}.


to_json(Req, State) ->
	%% @todo
	{<<"Body">>, Req, State}.


template_from_json(Req, State) ->
	%% @todo
	{true, Req, State}.
template_from_form(Req,State)->
	%% @todo
	{true, Req, State}.

delete_resource(Req, State) ->
	%% @todo
	{true, Req, State}.

delete_completed(Req, State) ->
	%% @todo
	{true, Req, State}.


%%====================================================================
%% Internal functions
%%====================================================================

