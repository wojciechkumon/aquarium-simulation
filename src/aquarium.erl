-module(aquarium).

-import(dispatcher, [startDispatcher/2]).
-import(time, [startTime/1]).
-import(screen, [clear_screen/0, printLastCommand/1, readLine/0]).

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
      screen:clear_screen();
    _ -> handleUserInput(DispatcherPid, Timer)
  end.
