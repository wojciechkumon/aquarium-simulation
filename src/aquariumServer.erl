-module(aquariumServer).

-export([startServer/2, defaultHost/0, defaultPort/0, tcpOptions/0]).

-define(INFO_LINE_TIMEOUT, 2000).
-define(TIMEOUT, 100).

defaultHost() -> "localhost".
defaultPort() -> 11111.
tcpOptions() -> [{active, false}, {packet, 2}].

startServer(ServerPort, {PrinterPid, DispatcherPid}) ->
  case gen_tcp:listen(ServerPort, tcpOptions()) of
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
      timer:send_after(?INFO_LINE_TIMEOUT, PrinterPid, clearInfoLine),
      spawn(fun() -> handleSocket(Socket, {PrinterPid, DispatcherPid}, self()) end),
      closeOrContinue(ServerSocket, {PrinterPid, DispatcherPid});
    {error, timeout} ->
      closeOrContinue(ServerSocket, {PrinterPid, DispatcherPid});
    {error, Error} ->
      PrinterPid ! {printInfo, io_lib:format("Connection accept error (~p)~n", [Error])},
      waitForClose()
  end.

closeOrContinue(ServerSocket, {PrinterPid, DispatcherPid}) ->
  receive
    {close, Pid} ->
      gen_tcp:close(ServerSocket),
      Pid ! closed
  after
    0 -> waitForConnection(ServerSocket, {PrinterPid, DispatcherPid})
  end.

handleSocket(Socket, {PrinterPid, DispatcherPid}, ServerPid) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, "checkAquariumState"} ->
      CurrentAquariumStateString = getCurrentAquariumState(DispatcherPid),
      gen_tcp:send(Socket, CurrentAquariumStateString),
      handleSocket(Socket, {PrinterPid, DispatcherPid}, ServerPid);
    {ok, "feed"} ->
      DispatcherPid ! feed,
      gen_tcp:send(Socket, "ok"),
      handleSocket(Socket, {PrinterPid, DispatcherPid}, ServerPid);
    {ok, "heaterHigh"} ->
      DispatcherPid ! {heater, high},
      gen_tcp:send(Socket, "ok"),
      handleSocket(Socket, {PrinterPid, DispatcherPid}, ServerPid);
    {ok, "heaterNormal"} ->
      DispatcherPid ! {heater, normal},
      gen_tcp:send(Socket, "ok"),
      handleSocket(Socket, {PrinterPid, DispatcherPid}, ServerPid);
    {ok, "heaterOff"} ->
      DispatcherPid ! {heater, off},
      gen_tcp:send(Socket, "ok"),
      handleSocket(Socket, {PrinterPid, DispatcherPid}, ServerPid);
    {ok, "closeConnection"} ->
      gen_tcp:send(Socket, "closing"),
      gen_tcp:close(Socket),
      ok;
    {error, Error} ->
      gen_tcp:close(Socket),
      PrinterPid ! {printInfo, io_lib:format("Connection error (~p)", [Error])},
      ok
  end.

getCurrentAquariumState(DispatcherPid) ->
  DispatcherPid ! {currentAquarium, self()},
  receive
    {currentAquarium, CurrentAquarium} ->
      stringConverter:toString(CurrentAquarium)
  end.