-module(dispatcher).

-import(printer, [printTime/2, clearFish/2, printAquariumState/1]).
-import(aquariumState, [refreshAquariumState/1]).

-export([startDispatcher/2]).

% Dispatcher process

startDispatcher(StartingFish, StartingAquariumState) ->
  StartingFishProcesses = spawnFish(StartingFish),
  dispatcherLoop(StartingFishProcesses, 0, StartingAquariumState).

dispatcherLoop(FishProcesses, Minutes, AquariumState) ->
  receive
    feed ->
      feed(FishProcesses),
      dispatcherLoop(FishProcesses, Minutes, AquariumState);
    newFish ->
      NewFishProcesses = addNewFishToList(FishProcesses),
      dispatcherLoop(NewFishProcesses, Minutes, AquariumState);
    timeStep ->
      {FishLeft, NewMinutes, NewAquariumState} = timeStep(FishProcesses, Minutes, AquariumState),
      dispatcherLoop(FishLeft, NewMinutes, NewAquariumState);
    {heater, Level} ->
      NewAquariumState = switchHeater(AquariumState, Level),
      dispatcherLoop(FishProcesses, Minutes, NewAquariumState)
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

timeStep(FishProcesses, Minutes, AquariumState) ->
  NewMinutes = (Minutes + 1) rem 1440,
  printTime(NewMinutes),
  FishLeft = refreshFish(NewMinutes, FishProcesses),
  clearDeadFishLines(FishProcesses, FishLeft),
  NewAquariumState = aquariumState:refreshAquariumState(AquariumState),
  printer:printAquariumState(NewAquariumState),
  {FishLeft, NewMinutes, NewAquariumState}.

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

switchHeater({Temperature, _}, Level) -> {Temperature, Level}.
