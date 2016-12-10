-module(aquarium).

-import(time, [startTime/1]).
-import(screen, [clearScreen/0, printLastCommand/1, readLine/0]).

-export([start/0]).

% Main process

start() ->
  screen:printBackground(),
  StartingFish = [neon, skalar],
  DispatcherPid = spawn(dispatcher, startDispatcher, [self(), StartingFish]),
  {_, Timer} = time:startTime(DispatcherPid),
  handleUserInput(DispatcherPid, Timer).

handleUserInput(DispatcherPid, Timer) ->
  Input = screen:readLine(),
  screen:printLastCommand(Input),
  case Input of
    "feed" ->
      DispatcherPid ! feed,
      handleUserInput(DispatcherPid, Timer);
    "newFish" ->
      DispatcherPid ! newFish,
      handleUserInput(DispatcherPid, Timer);
    "end" ->
      timer:cancel(Timer),
      screen:clearScreen();
    _ -> handleUserInput(DispatcherPid, Timer)
  end.
