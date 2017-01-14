-module(aquarium).

-export([start/0]).

% Main process

start() ->
  PrinterPid = spawn(printer, startPrinter, []),
  PrinterPid ! printBackground,
  StartingFish = [neon, danio],
  StartingAquariumState = aquariumState:startingAquariumState(),
  DispatcherPid = spawn(dispatcher, startDispatcher, [StartingFish, StartingAquariumState, PrinterPid]),
  Timer = time:startTime(DispatcherPid),
  Server = spawn(aquariumServer, startServer, [{PrinterPid, DispatcherPid}]),
  handleUserInput(DispatcherPid, PrinterPid, {Timer, Server}).

handleUserInput(DispatcherPid, PrinterPid, ToClose) ->
  Input = printer:readLine(),
  PrinterPid ! {printLastCommand, Input},
  case Input of
    "feed" ->
      DispatcherPid ! feed,
      handleUserInput(DispatcherPid, PrinterPid, ToClose);
    "neon" ->
      DispatcherPid ! {newFish, neon},
      handleUserInput(DispatcherPid, PrinterPid, ToClose);
    "danio" ->
      DispatcherPid ! {newFish, danio},
      handleUserInput(DispatcherPid, PrinterPid, ToClose);
    "guppy" ->
      DispatcherPid ! {newFish, guppy},
      handleUserInput(DispatcherPid, PrinterPid, ToClose);
    "algaeEater" ->
      DispatcherPid ! {newFish, algaeEater},
      handleUserInput(DispatcherPid, PrinterPid, ToClose);
    "heaterHigh" ->
      DispatcherPid ! {heater, high},
      handleUserInput(DispatcherPid, PrinterPid, ToClose);
    "heaterNormal" ->
      DispatcherPid ! {heater, normal},
      handleUserInput(DispatcherPid, PrinterPid, ToClose);
    "heaterOff" ->
      DispatcherPid ! {heater, off},
      handleUserInput(DispatcherPid, PrinterPid, ToClose);
    "clean" ->
      DispatcherPid ! clean,
      handleUserInput(DispatcherPid, PrinterPid, ToClose);
    "heal" ->
      DispatcherPid ! heal,
      handleUserInput(DispatcherPid, PrinterPid, ToClose);
    "end" ->
      cleanUp(ToClose, PrinterPid);
    _ -> handleUserInput(DispatcherPid, PrinterPid, ToClose)
  end.


cleanUp({Timer, Server}, PrinterPid) ->
  timer:cancel(Timer),
  Server ! {close, self()},
  receive
    closed -> ok
  end,
  PrinterPid ! clearScreen.