-module(dispatcher).

-import(screen, [printTime/2, clearFish/2]).

-export([startDispatcher/2]).

% Dispatcher process

startDispatcher(MainPid, StartingFish) ->
  StartingFishProcesses = spawnFish(StartingFish),
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
      {NewMinutes, FishLeft} = timeStep(FishProcesses, Minutes),
      dispatcherLoop(MainPid, FishLeft, NewMinutes)
  end.

spawnFish([]) -> [];
spawnFish([Fish | Tail]) ->
  [spawn(fish, startFish, [Fish]) | spawnFish(Tail)].

feed([]) -> ok;
feed([FishPid | Tail]) ->
  FishPid ! feed,
  feed(Tail).

addNewFishToList(FishProcesses) ->
  FishProcesses ++ [spawn(fish, startFish, [gupik])].

timeStep(FishProcesses, Minutes) ->
  NewMinutes = (Minutes + 1) rem 1440,
  printTime(NewMinutes),
  FishLeft = refreshFish(FishProcesses),
  clearDeadFishLines(FishProcesses, FishLeft),
  {NewMinutes, FishLeft}.

printTime(MinutesSum) ->
  Hours = MinutesSum div 60,
  Minutes = MinutesSum rem 60,
  screen:printTime(Hours, Minutes).

refreshFish(FishProcesses) -> refreshFish(FishProcesses, 0, []).

refreshFish([], _, List) -> List;
refreshFish([Fish | Tail], Number, List) ->
  Fish ! {refresh, Number, self()},
  receive
    {Fish, ok} -> refreshFish(Tail, Number + 1, List ++ [Fish]);
    {Fish, _} -> refreshFish(Tail, Number + 1, List) % death
  end.

clearDeadFishLines(FishProcesses, FishLeft) ->
  DeadFishAmount = length(FishProcesses) - length(FishLeft),
  screen:clearFish(length(FishLeft), DeadFishAmount).
