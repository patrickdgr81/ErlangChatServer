
-module(chatClient).
-behaviour(gen_server).

-export([start_client/0]).

start_client(Name) ->
	%% IMPORTANT: Start the empd daemon!
    os:cmd("epmd -daemon"),
    % format microseconds of timestamp to get an
    % effectively-unique node name
    case is_atom(Name) of 
    	true -> net_kernel:start([Name, shortnames]),
    			register(Name, self()),
    			{ok, Pid} = gen_server:start({local, ?MODULE}, ?MODULE, {}, []),
    			typeInput(Pid);
    	false -> net_kernel:start([list_to_atom(Name), shortnames]),
    			 register(Name, self()),
    			 {ok, Pid} = gen_server:start({local, ?MODULE}, ?MODULE, {}, []),
    			 typeInput(Pid)
    end.

%%% Server functions
init(_Args) ->  
    {ok, #server_state{activePids = [], history = []}}.

typeInput(Pid) ->
	String = io:get_line(">").