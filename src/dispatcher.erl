-module(dispatcher).

-import(fish, [startFish/2]).
-import(screen, [printTime/2]).

-export([startDispatcher/2]).

% Dispatcher process

startDispatcher(MainPid, StartingFish) ->
  StartingFishProcesses = spawnFish(StartingFish, self()),
  dispatcherLoop(MainPid, StartingFishProcesses, 0).

dispatcherLoop(MainPid, FishProcesses, Minutes) ->
  receive
    feed ->
      feed(FishProcesses),
      dispatcherLoop(MainPid, FishProcesses, Minutes);
    newFish ->
      NewFishProcesses = addNewFishToList(FishProcesses),
      dispatcherLoop(MainPid, NewFishProcesses, Minutes);
    timeStep ->
      dispatcherLoop(MainPid, FishProcesses, timeStep(FishProcesses, Minutes))
  end.

spawnFish([], _) -> [];
spawnFish([Fish | Tail], DispatcherPid) ->
  [spawn(fish, startFish, [Fish, DispatcherPid]) | spawnFish(Tail, DispatcherPid)].

feed([]) -> endOfFunction;
feed([FishPid | Tail]) ->
  FishPid ! feed,
  feed(Tail).

addNewFishToList(FishProcesses) ->
  FishProcesses ++ [spawn(fish, startFish, [gupik, self()])].

timeStep(FishProcesses, Minutes) ->
  NewMinutes = (Minutes + 1) rem 1440,
  printMinutes(NewMinutes),
  refreshFish(FishProcesses),
  NewMinutes.

printMinutes(MinutesSum) ->
  Hours = MinutesSum div 60,
  Minutes = MinutesSum rem 60,
  screen:printTime(Hours, Minutes).

refreshFish(FishProcesses) -> refreshFish(FishProcesses, 0).

refreshFish([], _) -> ok;
refreshFish([Fish | Tail], Number) ->
  Fish ! {refresh, Number},
  refreshFish(Tail, Number + 1).
