-module(aquariumClient).

-export([start/0, start/1, start/2]).

-define(TCP_OPTIONS, [{active, false}, {packet, 2}]).

%% Aquarium client main

start() ->
  start(aquariumServer:defaultPort()).

start(Port) ->
  start(aquariumServer:defaultHost(), Port).

start(Host, Port) ->
  Socket = connectWithServer(Host, Port),
  if
    Socket /= error ->
      io:format("Connected"),
      SocketHandler = spawn(socketHandlerForClient, handleSocket, [Socket]),
      Refresher = spawn(clientRefresher, startAquariumRefresher, [SocketHandler]),
      handleInput(SocketHandler, Refresher);
    true -> ok
  end.

connectWithServer(Host, Port) ->
  case gen_tcp:connect(Host, Port, aquariumServer:tcpOptions()) of
    {ok, Socket} ->
      Socket;
    {error, Error} ->
      io:format("Error while connecting with server (~p)~n", [Error]),
      error
  end.

handleInput(SocketHandler, Refresher) ->
  Input = readLine(),
  if
    Input == "end" ->
      handleEnd(SocketHandler, Refresher);
    true ->
      io:format("wrong command!~n"),
      handleInput(SocketHandler, Refresher)
  end.

readLine() ->
  string:strip(io:get_line(""), right, $\n).

handleEnd(SocketHandler, Refresher) ->
  Refresher ! {stop, self()},
  receive
    stopped -> ok
  end,
  SocketHandler ! {closeConnection, self()},
  receive
    closed -> ok
  end.