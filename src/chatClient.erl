
-module(chatClient).

-export([start_client/1,receiveMessage/0,typeInput/1]).

-define(NODE, chatServer@knuth).
-define(NODENAME, chatServer).

%%% Helper functions
receiveMessage() ->
    receive
        {message, M} -> io:format("~s~n", [M]);
        _ -> io:format("Got unexpected message~n")
    end,
    receiveMessage().

typeInput(SentString) ->
    String = io:get_line(">>>"),
    Message = SentString ++ ": " ++ String,
    io:format("Message: ~s~n",[Message]),
    {?NODENAME, ?NODE} ! {message, Message},
    typeInput(SentString).

start_client([Name, Computer]) ->
    %% IMPORTANT: Start the empd daemon!
    os:cmd("epmd -daemon"),
    
    net_kernel:start([Name, shortnames]),
    register(Name, self()),

    case net_adm:ping(?NODE) of
        pong ->
            SentString = list_to_atom(atom_to_list(Name) ++ "@" ++ atom_to_list(Computer)),
            gen_server:call({global, ?NODENAME}, {requestJoin, SentString}),
            spawn(?MODULE, typeInput, [SentString]),
            receiveMessage();
        pang ->
            io:format("Server is down at the moment...~n"),
            erlang:halt()
    end.
    



