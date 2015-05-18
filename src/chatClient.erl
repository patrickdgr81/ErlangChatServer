
-module(chatClient).

-export([start_client/1,receiveMessage/0]).

start_client(Name, Pid) ->
    %% IMPORTANT: Start the empd daemon!
    os:cmd("epmd -daemon"),
    % format microseconds of timestamp to get an
    % effectively-unique node name
    case is_atom(Name) of 
        true -> net_kernel:start([Name, shortnames]),
                register(Name, self());
        false -> net_kernel:start([list_to_atom(Name), shortnames]),
                 register(Name, self())
    end,
    Init = gen_server:call(Pid, requestJoin),
    case Init of
        ok -> io:format("!~p Connected to server~n", [now()]),
                     spawn(?MODULE, typeInput, Pid),
                     receiveMessage();
        _ -> io:format("!~p Failed to Create gen_server, exiting~n", [now()])
    end.

%%% Helper functions
receiveMessage() ->
    receive
        {message, M} -> io:format("~s~n", [M]);
        _ -> io:format("Got unexpected message~n")
    end,
    receiveMessage().

typeInput(Pid) ->
    String = io:get_line(">"),
    io:format("~s~n",[String]),
    Pid ! {message, String},
    typeInput(Pid).



