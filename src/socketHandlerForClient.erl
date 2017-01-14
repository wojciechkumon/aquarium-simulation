-module(socketHandlerForClient).

-export([handleSocket/1]).

%% Socket handler used in client

handleSocket(Socket) ->
  receive
    {checkAquariumState, Pid} ->
      io:format("[SocketHandler] checkAquariumState~n"),
      Response = sendRequest(Socket, "checkAquariumState"),
      Pid ! {response, Response},
      handleSocket(Socket);
    {closeConnection, Pid} ->
      io:format("[SocketHandler] closeConnection~n"),
      sendRequest(Socket, "closeConnection"),
      Pid ! closed
  end.

sendRequest(Socket, Message) ->
  case gen_tcp:send(Socket, [Message]) of
    {error, timeout} ->
      io:format("Send timeout, closing!~n", []),
      gen_tcp:close(Socket);
    {error, OtherSendError} ->
      io:format("Some other error on socket (~p), closing~n", [OtherSendError]),
      gen_tcp:close(Socket),
      connectionClosed;
    ok ->
      receiveResponse(Socket)
  end.

receiveResponse(Socket) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Data} ->
      io:format("Input ~p~n", [Data]),
      Data;
    {error, closed} ->
      io:format("Connection closed, err~n"),
      error
  end.