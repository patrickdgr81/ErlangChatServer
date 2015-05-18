-module(chatServer).
-behaviour(gen_server).

-export([start_server/0]).

-export([init/1, 
		 handle_call/3, 
		 handle_cast/2, 
		 handle_info/2, 
		 code_change/3, 
		 terminate/2]).

-record(server_state, {activePids,
                history}).

start_server() -> 
	%% IMPORTANT: Start the empd daemon!
    os:cmd("epmd -daemon"),
    %% Register yourself as chatServer
    net_kernel:start([chatServer, shortnames]),
    gen_server:start({local, ?MODULE}, ?MODULE, {}, []).

%%% Server functions
init(_Args) ->  
    {ok, #server_state{activePids = [], history = []}}.

code_change(_OldVsn, State, _Extra) ->
	%% No change planned. The function is there for the behaviour,
	%% but will not be used.
	{ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%% SYNCRHONOUS MESSAGES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%Getting the history of the server
handle_call(getHistory, _From, S) ->
	{reply, S#server_state.history, S};

%%A new client requested to join
handle_call(requestJoin, {Pid, _Tag}, S) ->
	%%Monitor the process in case it crashes. 
	%%If its gone, we remove it from the chatroom later
	io:format("!~p New Process in chatroom ~p~n", [now(), Pid]),
	erlang:monitor(process, Pid),
	{reply, ok, S#server_state{activePids = lists:append(S#server_state.activePids, [Pid])}}.

%%%%%%%%%%%%%%%%%%%%%%%%%% END SYNCRHONOUS MESSAGES %%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%% ASYNCRHONOUS MESSAGES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_cast({message, Message}, S) -> 
	io:format("!~p Got Message: ~p~n", [now(), Message]),
	tellEveryone(S#server_state.activePids, Message),
	{noreply, S#server_state{activePids = lists:append(S#server_state.history, [Message])}}.

%%Process exited or died on its own, we remove it from the list of active pids
handle_info({'DOWN', _MonitorRef, _Type, Pid, _Reason}, S) ->
	io:format("!~p Pid that died: ~p~n", [now(), Pid]),
	{noreply, S#server_state{activePids = lists:delete(Pid, S#server_state.activePids)}};

handle_info(Message,S) ->
	io:format("!~p Got Unexpected Message: ~p~n", [now(), Message]),
	{noreply, S}.

terminate(_Reason, _State) ->
    io:format("!~p Terminating, reason: ~p~n", [now(), _Reason]).

%%%%%%%%%%%%%%%%%%%%%%%%%% END ASYNCRHONOUS MESSAGES %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%% HELPER FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%Tell all active people in the chatroom about the new message
tellEveryone([], _) -> ok;
tellEveryone([Pid|T], M) -> Pid ! {message, M},
							tellEveryone(T,M).
