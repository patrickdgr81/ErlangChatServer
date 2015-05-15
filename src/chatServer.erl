-module(chatServer).
-behaviour(gen_server).

-export([start_server/1]).

-record(server_state, {activePids,
                history}).

%%% Server functions
init([]) -> 
	%% IMPORTANT: Start the empd daemon!
    os:cmd("epmd -daemon"),
    %% Register yourself as chatServer
    net_kernel:start([chatServer, shortnames]),    
    {ok, #server_state{activePids = [], history = []}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%% SYNCRHONOUS MESSAGES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%Getting the history of the server
handle_call(getHistory, _From, S) ->
	{reply, S#server_state.history, S}.

%%A new client requested to join
handle_call(requestJoin, {Pid, _Tag}, S) ->
	%%Monitor the process in case it crashes. 
	%%If its gone, we remove it from the chatroom
	{reply, ok, S#server_state{activePids = lists:append(S#server_state.history, [Pid])}}.

%%%%%%%%%%%%%%%%%%%%%%%%%% END SYNCRHONOUS MESSAGES %%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%% ASYNCRHONOUS MESSAGES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_cast({message, Message}, S) -> 
	io:format("!~p Got Message: ~p~n", [now(), Message]),
	tellEveryone(S#server_state.activePids, Message),
	{noreply, S#server_state{activePids = lists:append(S#server_state.history, [Pid])}}.

%%%%%%%%%%%%%%%%%%%%%%%%%% END ASYNCRHONOUS MESSAGES %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%% HELPER FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%Tell all active people in the chatroom about the new message
tellEveryone([], M) -> ok;
tellEveryone([Pid|T], M) -> Pid ! {message, M},
							tellEveryone(T,M).






	