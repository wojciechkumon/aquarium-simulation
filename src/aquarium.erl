-module(aquarium).

-import(time, [startTime/1]).
-import(printer, [readLine/0]).
-import(screen, [clearScreen/0]).
-import(aquariumState, [startingAquariumState/0]).

-export([start/0]).

% Main process

start() ->
  PrinterPid = spawn(printer, startPrinter, []),
  PrinterPid ! printBackground,
  StartingFish = [neon, danio],
  StartingAquariumState = aquariumState:startingAquariumState(),
  DispatcherPid = spawn(dispatcher, startDispatcher, [StartingFish, StartingAquariumState, PrinterPid]),
  {_, Timer} = time:startTime(DispatcherPid),
  handleUserInput(DispatcherPid, PrinterPid, Timer).

handleUserInput(DispatcherPid, PrinterPid, Timer) ->
  Input = printer:readLine(),
  PrinterPid ! {printLastCommand, Input},
  case Input of
    "feed" ->
      DispatcherPid ! feed,
      handleUserInput(DispatcherPid, PrinterPid, Timer);
    "neon" ->
      DispatcherPid ! {newFish, neon},
      handleUserInput(DispatcherPid, PrinterPid, Timer);
    "danio" ->
      DispatcherPid ! {newFish, danio},
      handleUserInput(DispatcherPid, PrinterPid, Timer);
    "guppy" ->
      DispatcherPid ! {newFish, guppy},
      handleUserInput(DispatcherPid, PrinterPid, Timer);
    "algaeEater" ->
      DispatcherPid ! {newFish, algaeEater},
      handleUserInput(DispatcherPid, PrinterPid, Timer);
    "heaterHigh" ->
      DispatcherPid ! {heater, high},
      handleUserInput(DispatcherPid, PrinterPid, Timer);
    "heaterNormal" ->
      DispatcherPid ! {heater, normal},
      handleUserInput(DispatcherPid, PrinterPid, Timer);
    "heaterOff" ->
      DispatcherPid ! {heater, off},
      handleUserInput(DispatcherPid, PrinterPid, Timer);
    "clean" ->
      DispatcherPid ! clean,
      handleUserInput(DispatcherPid, PrinterPid, Timer);
    "heal" ->
      DispatcherPid ! heal,
      handleUserInput(DispatcherPid, PrinterPid, Timer);
    "end" ->
      timer:cancel(Timer),
      screen:clearScreen();
    _ -> handleUserInput(DispatcherPid, PrinterPid, Timer)
  end.
