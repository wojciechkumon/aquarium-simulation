-module(socketHandlerForClient).

-export([handleSocket/2]).

%% Socket handler used in client

handleSocket(Socket, PrinterPid) ->
  receive
    {checkAquariumState, Pid} ->
      Response = sendRequest(Socket, PrinterPid, "checkAquariumState"),
      Pid ! {response, Response},
      handleSocket(Socket, PrinterPid);
    {feed, _} ->
      sendRequest(Socket, PrinterPid, "feed"),
      handleSocket(Socket, PrinterPid);
    {heater, high} ->
      sendRequest(Socket, PrinterPid, "heaterHigh"),
      handleSocket(Socket, PrinterPid);
    {heater, normal} ->
      sendRequest(Socket, PrinterPid, "heaterNormal"),
      handleSocket(Socket, PrinterPid);
    {heater, off} ->
      sendRequest(Socket, PrinterPid, "heaterOff"),
      handleSocket(Socket, PrinterPid);
    {closeConnection, Pid} ->
      sendRequest(Socket, PrinterPid, "closeConnection"),
      Pid ! closed
  end.

sendRequest(Socket, PrinterPid, Message) ->
  case gen_tcp:send(Socket, [Message]) of
    {error, timeout} ->
      PrinterPid ! {printInfo, "Send timeout, closing!"},
      gen_tcp:close(Socket);
    {error, _} ->
      gen_tcp:close(Socket),
      connectionClosed;
    ok ->
      receiveResponse(Socket, PrinterPid)
  end.

receiveResponse(Socket, PrinterPid) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Data} ->
      Data;
    {error, closed} ->
      PrinterPid ! {printInfo, "Connection closed"},
      error
  end.