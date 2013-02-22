-module(file_handler2).

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
	{[<<"GET">>, <<"POST">>, <<"DELETE">>], Req, State}.

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
		{<<"application/json">>, template_from_json}
	], Req, State}.



to_html(Req, State) ->
	io:format(" req ...... ~p~n",[Req]),
	     Body = <<"html, body {
	margin: 16px;
	padding: 0;
	font: 12pt Tahoma,Arial,Helvetica,Myriad,clean,sans-serif; 
}

header {
	text-align: center; 
	margin: 0;
	padding: 0;
    border: 0;
    filter: progid:DXImageTransform.Microsoft.gradient(startColorstr='#ffffff', endColorstr='#f3efe3'); /* IE */
    background: -webkit-gradient(linear, left top, left bottom, from(#ffffff), to(#f3efe3));
    background: -moz-linear-gradient(top,  #ffffff,  #f3efe3);
    border-bottom: 1px solid #f3efe3;
}

h1 {
	font-size:1.5em;
	font-weight: bold;
	color: #DB750F;
}

h2 {
	font-size: 1.1em;
	font-weight: bold;
	color: #944F0A;
}

.navigation {
  height:75%; width:125px; 
  color:white;
  border-color:maroon; 
  border-style:solid; 
  border-width:1px; 
  float:left; 
  background-color:#4852B7
}

footer{
	text-align: center; 
	border-top: 1px solid #f3efe3;
}">>,
       case cowboy_req:qs_val(<<"parts">>, Req) of
		{undefined, Req2} -> {Body, Req2, State};
		{_, Req2} ->
			Body2 = <<"{\"ExportLocation\":\"/xfer/export/sankar/Hello World/hello.css.zip\",\"Name\":\"hello.css\",\"SearchLocation\":\"/filesearch?q=\",\"ETag\":\"47fc97b3caf4be1e333af5bc265c0ecc3bfb5ca\",\"Attributes\":{\"ReadOnly\":false,\"Archive\":true,\"Hidden\":false,\"SymLink\":false},\"Length\":888,\"LocalTimeStamp\":1359962562458,\"Location\":\"/orion/file/sankar/Hello World/hello.css\",\"ImportLocation\":\"/xfer/import/sankar/Hello World/hello.css\",\"Parents\":[{\"Name\":\"Hello World\",\"ChildrenLocation\":\"/orion/file/sankar/Hello World/?depth=1\",\"Location\":\"/orion/file/sankar/Hello World/\"}],\"Directory\":false}">>,
			{Body2, Req2, State}
	end.

to_json(Req, State) ->
	%% @todo
	Body = <<"{success}">>,
	{Body, Req, State}.

%% Internal.

template_from_json(Req, State) ->
	%% @todo
	Body =  <<"{
		  \"Id\": \"A\",
		  \"Name\": \"sankar\",
		  \"SearchLocation\": \"/filesearch?q=\",
		  \"UserName\": \"sankar\",
		  \"Workspaces\": [{
		    \"Id\": \"sankar\",
		    \"LastModified\": 1359697344754,
		    \"Location\": \"/orion/workspace/sankar\",
		    \"Name\": \"Orion Content\"
		  }]
		}">>,
	{Body, Req, State}.

%% @todo Later just send JSON from form directly.
template_from_form(Req, State) ->
io:format("inside request %%%%%%%%%%%%%%%%%%%%%%%% ~p~n",[Req]),
Body =  <<"{
		  \"Id\": \"A\",
		  \"Name\": \"sankar\",
		  \"SearchLocation\": \"/filesearch?q=\",
		  \"UserName\": \"sankar\",
		  \"Workspaces\": [{
		    \"Id\": \"sankar\",
		    \"LastModified\": 1359697344754,
		    \"Location\": \"/orion/workspace/sankar\",
		    \"Name\": \"Orion Content\"
		  }]
		}">>,
	{Body, Req, State}.
	
process_post(Req, State) ->
	
	io:format("inside request %%%%%%%%%%%%%%%%%%%%%%%% ~p~n",[Req]),
        
	Body =  <<"{
		  \"Id\": \"A\",
		  \"Name\": \"sankar\",
		  \"SearchLocation\": \"/filesearch?q=\",
		  \"UserName\": \"sankar\",
		  \"Workspaces\": [{
		    \"Id\": \"sankar\",
		    \"LastModified\": 1359697344754,
		    \"Location\": \"/orion/workspace/sankar\",
		    \"Name\": \"Orion Content\"
		  }]
		}">>,
	Req2 = cowboy_req:set_resp_body(Body, Req),		
	Req3 = cowboy_req:set_resp_header(<<"content-type">>,<<"application/json">>,Req2),
	{true, Req3, State}.