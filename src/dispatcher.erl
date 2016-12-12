-module(dispatcher).

-import(printer, [printTime/2, clearFish/2]).

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
  FishLeft = refreshFish(NewMinutes, FishProcesses),
  clearDeadFishLines(FishProcesses, FishLeft),
  {NewMinutes, FishLeft}.

printTime(MinutesSum) ->
  Hours = MinutesSum div 60,
  Minutes = MinutesSum rem 60,
  printer:printTime(Hours, Minutes).

refreshFish(Minutes, FishProcesses) -> refreshFish(Minutes, FishProcesses, 0, []).

refreshFish(_, [], _, List) -> List;
refreshFish(Minutes, [Fish | Tail], Number, List) ->
  Hour = Minutes div 60,
  Fish ! {refresh, Hour, Number, self()},
  receive
    {Fish, ok} -> refreshFish(Minutes, Tail, Number + 1, List ++ [Fish]);
    {Fish, _} -> refreshFish(Minutes, Tail, Number + 1, List) % death
  end.

clearDeadFishLines(FishProcesses, FishLeft) ->
  DeadFishAmount = length(FishProcesses) - length(FishLeft),
  printer:clearFish(length(FishLeft), DeadFishAmount).
