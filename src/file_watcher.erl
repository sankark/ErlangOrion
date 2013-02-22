-module(file_watcher).

-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0,notify/1,is_modified/2]).

-include_lib("kernel/include/file.hrl").

-record(state,{file,callback,last_poll_time}).

%%====================================================================
%% API
%%====================================================================
%% @spec start_link(SiteProps) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the notification server
start_link()->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% API for notification
%% Calls are done in the calling process, to prevent copying of 
%% possibly large contexts for small notifications.
%%====================================================================

%% @doc Cast the event to all observers. The prototype of the observer is: f(Msg, Context) -> void
notify(State=#state{file=FileName,callback=CallBack,last_poll_time=Poll}) ->
     NewPol = case ?MODULE:is_modified(FileName,Poll) of
		{true,NewPoll} -> io:format("file modified"),
							CallBack([]),
						  NewPoll;
		{false,NewPoll}      ->NewPoll
	end,
	State#state{last_poll_time=NewPol}.
    
    

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server, creates a new observer list
init([]) ->
	FileName=orion_config:get(dispatch_file),
	CallBack=fun mod_dispatch:update/1,
    State = #state{file=FileName,callback=CallBack,last_poll_time=0},
	timer:send_interval(5000, {tick}),
    {ok, State}.


%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.


%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.


%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handle timer ticks
handle_info({tick}, State) ->
   State2 = ?MODULE:notify(State),
    {noreply, State2};

handle_info(_Info, State) ->
    {noreply, State}.


%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(normal, _State) ->
    ok;
terminate(_Reason, _State) ->
    ok.


%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ----------------------------------------------

is_modified(Filename, LastPollTime) ->
    NewPollTime = calendar:datetime_to_gregorian_seconds(
        { date(), time() }
    ),
        {ok, FileInfo} = file:read_file_info(Filename),
	    IsModified = calendar:datetime_to_gregorian_seconds(
				FileInfo#file_info.mtime 
			) > LastPollTime,
		{IsModified,NewPollTime}.