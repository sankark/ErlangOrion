-module(users_handler).

-export([init/3]).
-export([rest_init/2]).
-export([is_authorized/2]).
-export([content_types_provided/2]).
-export([charsets_provided/2]).
-export([to_html/2]).
-export([to_json/2]).
-export([collection_to_html/2]).
-export([collection_to_json/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([process_post/2]).
-export([template_from_json/2]).
-export([template_from_form/2]).
-export([resource_exists/2]).

-define(BUCKET,<<"users">>).

-record(state, {
	auth = undefined,
	collection = undefined :: boolean(),
	userid = undefined,
	template,
	key,
	get_rules,
	put_rules,
	data,
	bucket,
	collection_template,
	list_keys
}).

init(_, _, _) ->
	{upgrade, protocol, cowboy_rest}.

rest_init(Req, Opts) ->
	State = case lists:keyfind(auth, 1, Opts) of
		{auth, AuthOpts} -> #state{auth=AuthOpts};
		false -> #state{}
	end,
	Template = proplists:get_value(template, Opts, []),
	Coll_Template = proplists:get_value(collection_template, Opts, []),
	List_Keys = proplists:get_value(list_keys, Opts, true),
	Get_Rules = proplists:get_value(get_rules, Opts, []),
	Put_Rules = proplists:get_value(put_rules, Opts, []),
	Bucket = proplists:get_value(bucket, Opts, ?BUCKET),
	{Name, Req2} = cowboy_req:path_info(Req),
	io:format("Path info~p",[Name]),
	{ok, Req2, State#state{bucket = Bucket,key = Name, put_rules = Put_Rules, list_keys = List_Keys,collection=Name =:= [],template = Template, get_rules = Get_Rules, collection_template = Coll_Template}}.

is_authorized(Req, State) ->
	{true, Req, State}.


allowed_methods(Req, State) ->
	{[<<"GET">>, <<"PUT">>,<<"DELETE">>], Req, State}.


content_types_provided(Req, State=#state{collection=true}) ->
	{[
		{<<"text/html">>, collection_to_html},
		{<<"application/json">>, collection_to_json}
	], Req, State};
content_types_provided(Req, State=#state{collection=false}) ->
	{[
		{<<"text/html">>, to_html},
		{<<"application/json">>, to_json}
	], Req, State}.


%% Only allow UTF-8.
charsets_provided(Req, State) ->
	{[{<<"utf-8">>, 1000}, {<<"*">>, 0}], Req, State}.

content_types_accepted(Req, State=#state{collection=false}) ->
	{[
		{<<"application/x-www-form-urlencoded; charset=UTF-8">>,
			template_from_form},
		{<<"text/plain; charset=UTF-8">>,
			template_from_form},
		{<<"application/json; charset=UTF-8">>, template_from_json}
	], Req, State}.


resource_exists(Req, State=#state{get_rules=Rules,collection=true,list_keys=false}) when Rules /= [] ->
	case query_all_values(Rules, Req) of
		{ok, Data, Req2} ->
			{true, Req2, State#state{data=Data}};
		{error, notfound} ->
			{false, Req, State}
	end;

resource_exists(Req, State=#state{get_rules=Rules,collection=true,list_keys=true}) when Rules /= [] ->
	case query_all(Rules, Req) of
		{ok, Data, Req2} ->
			{true, Req2, State#state{data=Data}};
		{error, notfound} ->
			{false, Req, State}
	end;

resource_exists(Req, State=#state{collection=true, bucket = Bucket}) ->
	case orion_data_server:get_keys(Bucket) of
		{ok, Data} -> {true, Req, State#state{data=Data}};
		{error, notfound} -> {false, Req, State#state{data=[]}}
	end;


resource_exists(Req, State=#state{get_rules=Rules}) when Rules =/= []->
	case query_data(Rules, Req) of
		{ok, Data, Req2} ->
			{true, Req2, State#state{data=Data}};
		{error, notfound} ->
			{false, Req, State}
	end;


resource_exists(Req, State=#state{key = Key, bucket =Bucket})->
	case orion_data_server:get_value(Bucket, Key) of
		{ok, Data} -> {true, Req, State#state{data=Data}};
		{error, notfound} -> {false, Req, State#state{data=[]}}
	end.



to_html(Req, State=#state{template=Template,data=Data}) when Template /= []  ->
	T = list_to_atom(atom_to_list(Template)++"_dtl"),
	{ok, Body} = T:render(Data),
	{Body, Req, State};

to_html(Req, State) ->
	to_json(Req,State).

to_json(Req, State=#state{data = Data}) ->
	Body = jsx:encode(Data),
	{Body, Req, State}.

collection_to_html(Req, State=#state{collection_template=Coll_Template,data=Data}) when Coll_Template /= [] ->
	T = list_to_atom(atom_to_list(Coll_Template)++"_dtl"),
	{ok, Body} = T:render(Data),
	{Body, Req, State};

collection_to_html(Req, State) ->
	collection_to_json(Req,State).

collection_to_json(Req, State=#state{data = Data}) ->
	Body = jsx:encode(Data),
	{Body, Req, State}.

template_from_json(Req, State) ->
template_from_form(Req,State).

template_from_form(Req, State=#state{put_rules = Rules,key = Key,bucket = Bucket}) when Rules =:= []-> 
io:format("inside request %%%%%%%%%%%%%%%%%%%%%%%% ~p~n",[Req]),
	%%{Path_Info, Req1} = cowboy_req:path_info(Req),
	{ok, Values, Req2} = cowboy_req:body_qs(Req),
	{<<"login">>, Login} = lists:keyfind(<<"login">>, 1, Values),
	Body = case Key of
			[<<"new">>] -> case orion_data_server:get_data(Bucket, Login) of
							 {ok, _Data} -> <<"User Already Exist">>;
							{error, notfound} -> set_data(Bucket,Values, Login),
												                Body2 = <<"{\"lastlogintimestamp\":1359725142013,\"uid\":\"E\",\"Plugins\": [],\"properties\": {},\"hasPassword\": true,\"email\": \"gddds@dgvrd.com\",\"emailConfirmed\": true,\"login\":\"sxdfgdfg\",\"Location\":\"/users/E\",\"Name\":\"sxdfgdfg\",\"CanAddUsers\":true,\"RegistrationURI\":\"http://test/f\",\"sankar\":\"true\",\"ForceEmail\":false,\"emailConfigured\":\"true\"}">>,
												                Body2
						  end;
					_ -> case orion_data_server:get_data(Bucket, Key) of
						 {ok, _Data} -> set_data(Bucket,Values, Key),
										<<"Update done">>;
						{error, notfound} ->  <<"User Not Exist">>
						 end
			end,
	
%%orion_userdata_server:set_data(Bucket, Key, EditBy, UserData, Comments) 
%%orion_userdata_server:set_data(<<"userdata_profiles">>, Key, EditBy, UserData, Comments), 
%%{ok, Data} = orion_userdata_server:get_data(Bucket, Key),
%%{<<"value">>, UserData} = lists:keyfind(<<"value">>, 1, Data),

	send_response(State,Req2,Body);

template_from_form(Req, State=#state{put_rules = Rules}) -> 
io:format("inside request %%%%%%%%%%%%%%%%%%%%%%%% ~p~n",[Req]),
	process_rules(Rules,Req),
	Body = <<"">>,
	send_response(State,Req,Body).

set_data(Bucket,Values, Login) ->
	
    orion_data_server:set_data(Bucket, Login, Login, jsx:encode(Values), Login).

send_response(State, Req3, Body) ->
	Req4 = cowboy_req:set_resp_body(Body,Req3),
    {true, Req4, State}.
	
process_post(Req, State) ->
	{ok, Values, _Req3} = cowboy_req:body_qs(Req),
	io:format("Values %%%%%%%%%%%%%%%%%%%%%%%% ~p~n",[Values]),
	Req2 = cowboy_req:set_resp_body(<<"{\"lastlogintimestamp\":1359725142013,\"uid\":\"E\",\"Plugins\": [],\"properties\": {},\"hasPassword\": true,\"email\": \"gddds@dgvrd.com\",\"emailConfirmed\": true,\"login\":\"sxdfgdfg\",\"Location\":\"/users/E\",\"Name\":\"sxdfgdfg\",\"CanAddUsers\":true,\"RegistrationURI\":\"http://test/f\",\"sankar\":\"true\",\"ForceEmail\":false,\"emailConfigured\":\"true\"}">>, Req),		
	Req4 = cowboy_req:set_resp_header(<<"content-type">>,<<"application/json">>,Req2),
	{true, Req4, State}.


process_rules(Rules,Req) ->
	process_rules(Rules,Req,[]).

process_rules([],_Req,_Acc) ->
	[];

process_rules([Rule|Tail],Req,Acc) ->
	case rule(Rule,Req) of
		E = {error, _} ->
			E;
		{Field, Req2} ->
			process_rules(Tail, Req2, [Field|Acc])
	end.

rule({StatusFld,{put,Bucket,{binding,Binding}}},Req) ->
	Bucket2 = atom_to_binary(Bucket, latin1),
	{Key, Req2} = cowboy_req:binding(Binding, Req),
	{ok, Values, Req4} = cowboy_req:body_qs(Req2),
	case orion_userdata_server:set_data(Bucket2,Key,<<"test">>,jsx:encode(Values),<<"test">>) of
		ok ->
			{{StatusFld, <<"Successfully inserted/updated">>}, Req4};
		E = {error, _} ->
			E
	end;

rule({StatusFld,{put,Bucket,{binding,Binding},{qs_property,Prop}}},Req) ->
	Bucket2 = atom_to_binary(Bucket, latin1),
	{Key, Req2} = cowboy_req:binding(Binding, Req),
	{ok, Values, Req4} = cowboy_req:body_qs(Req2),
	{Prop, Value} = lists:keyfind(Prop, 1, Values),
	case orion_userdata_server:set_data(Bucket2,Key,<<"test">>,jsx:encode(Value),<<"test">>) of
		ok ->
			{{StatusFld, <<"Successfully inserted/updated">>}, Req4};
		E = {error, _} ->
			E
	end;
rule({StatusFld,{put,Bucket,{qs_binding,Binding},{qs_property,Prop}}},Req) ->
	Bucket2 = atom_to_binary(Bucket, latin1),
	{ok, Values, Req4} = cowboy_req:body_qs(Req),
	{Binding, Key} = lists:keyfind(Binding, 1, Values),
	{Prop, Value} = lists:keyfind(Prop, 1, Values),
	case orion_userdata_server:set_data(Bucket2,Key,<<"test">>,jsx:encode(Value),<<"test">>) of
		ok ->
			{{StatusFld, <<"Successfully inserted/updated">>}, Req4};
		E = {error, _} ->
			E
	end;

rule({redirect,URL},Req) ->
	{{redirect,URL},Req}.

query_all_values(Rules, Req) ->
	query_all_values(Rules, Req, []).
query_all_values([], Req, Acc) ->
	{ok, Acc, Req};
query_all_values([Rule|Tail], Req, Acc) ->
	case query_field_values(Rule, Req) of
		E = {error, _} ->
			E;
		{Field, Req2} ->
			query_all(Tail, Req2, [Field|Acc])
	end.


query_field_values({Name, {get, Bucket, {binding, _Binding}}}, Req) ->
	Bucket2 = atom_to_binary(Bucket, latin1),
	case orion_data_server:get_all_values(Bucket2) of
		{ok, JSON} ->
			{{Name, jsx:decode(JSON)}, Req};
		E = {error, _} ->
			E
	end;
query_field_values({Name, {get, Bucket, path_info}}, Req) ->
	Bucket2 = atom_to_binary(Bucket, latin1),
	case orion_data_server:get_all_values(Bucket2) of
		{ok, JSON} ->
			{{Name, jsx:decode(JSON)}, Req};
		E = {error, _} ->
			E
	end;
query_field_values({Name, {mfa, {M, F, A}}}, Req) ->
	{ok, ValuesList} = apply(M, F, A),
	{{Name, ValuesList}, Req}.

query_all(Rules, Req) ->
	query_all(Rules, Req, []).
query_all([], Req, Acc) ->
	{ok, Acc, Req};
query_all([Rule|Tail], Req, Acc) ->
	case query_field_all(Rule, Req) of
		E = {error, _} ->
			E;
		{Field, Req2} ->
			query_all(Tail, Req2, [Field|Acc])
	end.


query_field_all({Name, {get, Bucket, {binding, _Binding}}}, Req) ->
	Bucket2 = atom_to_binary(Bucket, latin1),
	case orion_data_server:get_keys(Bucket2) of
		{ok, JSON} ->
			{{Name, jsx:decode(JSON)}, Req};
		E = {error, _} ->
			E
	end;
query_field_all({Name, {get, Bucket, path_info}}, Req) ->
	Bucket2 = atom_to_binary(Bucket, latin1),
	case orion_data_server:get_keys(Bucket2) of
		{ok, JSON} ->
			{{Name, jsx:decode(JSON)}, Req};
		E = {error, _} ->
			E
	end;
query_field_all({Name, {mfa, {M, F, A}}}, Req) ->
	{ok, ValuesList} = apply(M, F, A),
	{{Name, ValuesList}, Req}.

query_data(Rules, Req) ->
	query_data(Rules, Req, []).
query_data([], Req, Acc) ->
	{ok, Acc, Req};
query_data([Rule|Tail], Req, Acc) ->
	case query_field(Rule, Req) of
		E = {error, _} ->
			E;
		{Field, Req2} ->
			query_data(Tail, Req2, [Field|Acc])
	end.

query_field({Name, {get, Bucket, {binding, Binding}}}, Req) ->
	Bucket2 = atom_to_binary(Bucket, latin1),
	{Key, Req2} = cowboy_req:binding(Binding, Req),
	case orion_data_server:get_value(Bucket2, Key) of
		{ok, JSON} ->
			{{Name, jsx:decode(JSON)}, Req2};
		E = {error, _} ->
			E
	end;
query_field({Name, {get, Bucket, path_info}}, Req) ->
	Bucket2 = atom_to_binary(Bucket, latin1),
	{Key, Req2} = cowboy_req:path_info(Req),
	case orion_userdata_server:get_value(Bucket2, Key) of
		{ok, JSON} ->
			{{Name, jsx:decode(JSON)}, Req2};
		E = {error, _} ->
			E
	end;

query_field({Name, {get_all, Bucket}}, Req) ->
	Bucket2 = atom_to_binary(Bucket, latin1),
	{ok, ValuesList} = orion_userdata_server:get_all_values(Bucket2),
	{{Name, ValuesList}, Req};
query_field({Name, {mfa, {M, F, A}}}, Req) ->
	{ok, ValuesList} = apply(M, F, A),
	{{Name, ValuesList}, Req}.

