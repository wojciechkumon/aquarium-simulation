-module(aquariumServer).

-export([startServer/1, startServer/2, defaultHost/0, defaultPort/0, tcpOptions/0]).

-define(TIMEOUT, 100).

defaultHost() -> "localhost".
defaultPort() -> 11111.
tcpOptions() -> [{active, false}, {packet, 2}].

startServer(PrinterPid) ->
  startServer(PrinterPid, defaultPort()).

startServer(PrinterPid, Port) ->
  case gen_tcp:listen(Port, tcpOptions()) of
    {ok, ServerSocket} -> waitForConnection(ServerSocket, PrinterPid);
    {error, Error} -> PrinterPid ! {printInfo, io_lib:format("Server error (~p)", [Error])}
  end.

waitForConnection(ServerSocket, PrinterPid) ->
  case gen_tcp:accept(ServerSocket, ?TIMEOUT) of
    {ok, Socket} ->
      PrinterPid ! {printInfo, "new connection"},
      spawn(fun() -> handleSocket(Socket, PrinterPid) end),
      closeOrContinue(ServerSocket, PrinterPid);
    {error, timeout} ->
      closeOrContinue(ServerSocket, PrinterPid);
    {error, Error} ->
      PrinterPid ! {printInfo, io_lib:format("Connection accept error (~p)~n", [Error])}
  end.

closeOrContinue(ServerSocket, PrinterPid) ->
  receive
    {close, Pid} ->
      ShouldClose = true,
      gen_tcp:close(ServerSocket),
      Pid ! closed
  after
    0 -> ShouldClose = false
  end,
  if
    ShouldClose == true ->
      ok;
    true -> waitForConnection(ServerSocket, PrinterPid)
  end.

handleSocket(Socket, PrinterPid) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, "closeConnection"} ->
      gen_tcp:send(Socket, "closing"),
      gen_tcp:close(Socket),
      ok;
    {ok, "checkAquariumState"} ->
      PrinterPid ! {printInfo, io_lib:format("Input ~p~n", ["checkAquariumState"])},
      gen_tcp:send(Socket, "superAquariumState, 10 fish!"),
      handleSocket(Socket, PrinterPid);
    {error, Error} ->
      PrinterPid ! {printInfo, io_lib:format("Connection error (~p)", [Error])},
      ok
  end.
