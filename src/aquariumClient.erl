-module(aquariumClient).

-export([start/0, start/1, start/2]).

-define(TCP_OPTIONS, [{active, false}, {packet, 2}]).

%% Aquarium client main

start() ->
  start(aquariumServer:defaultPort()).

start(Port) ->
  start(aquariumServer:defaultHost(), Port).

start(Host, Port) ->
  PrinterPid = spawn(printer, startPrinter, []),
  PrinterPid ! printClientBackground,
  Socket = connectWithServer(Host, Port, PrinterPid),
  if
    Socket /= error ->
      SocketHandler = spawn(socketHandlerForClient, handleSocket, [Socket, PrinterPid]),
      Refresher = spawn(clientRefresher, startAquariumRefresher, [SocketHandler, PrinterPid]),
      handleInput(SocketHandler, Refresher, PrinterPid);
    true -> ok
  end.

connectWithServer(Host, Port, PrinterPid) ->
  case gen_tcp:connect(Host, Port, aquariumServer:tcpOptions()) of
    {ok, Socket} ->
      Socket;
    {error, Error} ->
      PrinterPid ! clearScreen,
      Message = io_lib:format("Error while connecting with server (~p)", [Error]),
      PrinterPid ! {printInfo, Message},
      error
  end.

handleInput(SocketHandler, Refresher, PrinterPid) ->
  Input = printer:readLine(),
  if
    Input == "end" ->
      handleEnd(SocketHandler, Refresher, PrinterPid);
    true ->
      PrinterPid ! {printInfo, "wrong command!"},
      handleInput(SocketHandler, Refresher, PrinterPid)
  end.

handleEnd(SocketHandler, Refresher, PrinterPid) ->
  Refresher ! {stop, self()},
  receive
    stopped -> ok
  end,
  SocketHandler ! {closeConnection, self()},
  receive
    closed -> ok
  end,
  PrinterPid ! clearScreen.