-module(aquariumServer).

-export([startServer/1, startServer/2, defaultHost/0, defaultPort/0, tcpOptions/0]).

-define(TIMEOUT, 100).

defaultHost() -> "localhost".
defaultPort() -> 11111.
tcpOptions() -> [{active, false}, {packet, 2}].

startServer(Pids) ->
  startServer(Pids, defaultPort()).

startServer({PrinterPid, DispatcherPid}, Port) ->
  case gen_tcp:listen(Port, tcpOptions()) of
    {ok, ServerSocket} -> waitForConnection(ServerSocket, {PrinterPid, DispatcherPid});
    {error, Error} ->
      PrinterPid ! {printInfo, io_lib:format("Server error (~p)", [Error])},
      waitForClose()
  end.

waitForClose() ->
  receive
    {close, Pid} -> Pid ! closed
  end.

waitForConnection(ServerSocket, {PrinterPid, DispatcherPid}) ->
  case gen_tcp:accept(ServerSocket, ?TIMEOUT) of
    {ok, Socket} ->
      PrinterPid ! {printInfo, "new connection"},
      spawn(fun() -> handleSocket(Socket, {PrinterPid, DispatcherPid}) end),
      closeOrContinue(ServerSocket, {PrinterPid, DispatcherPid});
    {error, timeout} ->
      closeOrContinue(ServerSocket, {PrinterPid, DispatcherPid});
    {error, Error} ->
      PrinterPid ! {printInfo, io_lib:format("Connection accept error (~p)~n", [Error])}
  end.

closeOrContinue(ServerSocket, {PrinterPid, DispatcherPid}) ->
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
    true -> waitForConnection(ServerSocket, {PrinterPid, DispatcherPid})
  end.

handleSocket(Socket, {PrinterPid, DispatcherPid}) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, "closeConnection"} ->
      gen_tcp:send(Socket, "closing"),
      gen_tcp:close(Socket),
      ok;
    {ok, "checkAquariumState"} ->
      PrinterPid ! {printInfo, io_lib:format("Input ~p~n", ["checkAquariumState"])},
      CurrentAquariumStateString = getCurrentAquariumState(DispatcherPid),
      gen_tcp:send(Socket, CurrentAquariumStateString),
      handleSocket(Socket, {PrinterPid, DispatcherPid});
    {error, Error} ->
      PrinterPid ! {printInfo, io_lib:format("Connection error (~p)", [Error])},
      ok
  end.

getCurrentAquariumState(DispatcherPid) ->
  DispatcherPid ! {currentAquarium, self()},
  receive
    {currentAquarium, CurrentAquarium} ->
      stringConverter:toString(CurrentAquarium)
  end.