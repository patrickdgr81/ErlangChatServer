
-module(chatClient).

-export([start_client/1,receiveMessage/0]).

-define(SERVERPID, chatServerStart@knuth).
-define(SERVER, chatServer).


start_client(Name, Computer, Pid) ->
    %% IMPORTANT: Start the empd daemon!
    os:cmd("epmd -daemon"),
    
    net_kernel:start([Name, shortnames]),
    register(Name, self());

    case net_adm:ping(?SERVERPID) of
        pong ->
            SentString = list_to_atom(Name ++ "@" ++ Computer),
            gen_server:call({global, ?SERVER}, {requestJoin, SentString}),
            spawn(?MODULE, typeInput, ),
            receiveMessage();
        pang ->
            io:format("Server is down at the moment...~n"),
            erlang:halt()
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



