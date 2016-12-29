-module(dispatcher).

-import(aquariumState, [refreshAquariumState/2, clean/1]).

-export([startDispatcher/3]).

% Dispatcher process

startDispatcher(StartingFish, StartingAquariumState, PrinterPid) ->
  StartingFishProcesses = spawnFish(StartingFish, PrinterPid),
  dispatcherLoop(StartingFishProcesses, 0, StartingAquariumState, PrinterPid).

dispatcherLoop(FishProcesses, Minutes, AquariumState, PrinterPid) ->
  receive
    feed ->
      feed(FishProcesses),
      dispatcherLoop(FishProcesses, Minutes, AquariumState, PrinterPid);
    newFish ->
      NewFishProcesses = addNewFishToList(FishProcesses, PrinterPid),
      dispatcherLoop(NewFishProcesses, Minutes, AquariumState, PrinterPid);
    timeStep ->
      {FishLeft, NewMinutes, NewAquariumState} = timeStep(FishProcesses, Minutes, AquariumState, PrinterPid),
      dispatcherLoop(FishLeft, NewMinutes, NewAquariumState, PrinterPid);
    {heater, Level} ->
      NewAquariumState = switchHeater(AquariumState, Level),
      dispatcherLoop(FishProcesses, Minutes, NewAquariumState, PrinterPid);
    clean ->
      NewAquariumState = aquariumState:clean(AquariumState),
      dispatcherLoop(FishProcesses, Minutes, NewAquariumState, PrinterPid)
  end.

spawnFish([], _) -> [];
spawnFish([Fish | Tail], PrinterPid) ->
  [spawn(fish, startFish, [Fish, PrinterPid]) | spawnFish(Tail, PrinterPid)].

feed([]) -> ok;
feed([FishPid | Tail]) ->
  FishPid ! feed,
  feed(Tail).

addNewFishToList(FishProcesses, PrinterPid) ->
  FishProcesses ++ [spawn(fish, startFish, [gupik, PrinterPid])].

timeStep(FishProcesses, Minutes, AquariumState, PrinterPid) ->
  NewMinutes = (Minutes + 1) rem 1440,
  printTime(NewMinutes, PrinterPid),
  FishLeft = refreshFish(NewMinutes, FishProcesses),
  clearDeadFishLines(FishProcesses, FishLeft, PrinterPid),
  NewAquariumState = aquariumState:refreshAquariumState(AquariumState, length(FishLeft)),
  PrinterPid ! {printAquariumState, NewAquariumState},
  {FishLeft, NewMinutes, NewAquariumState}.

printTime(MinutesSum, PrinterPid) ->
  Hours = MinutesSum div 60,
  Minutes = MinutesSum rem 60,
  PrinterPid ! {printTime, Hours, Minutes}.

refreshFish(Minutes, FishProcesses) -> refreshFish(Minutes, FishProcesses, 0, []).

refreshFish(_, [], _, List) -> List;
refreshFish(Minutes, [Fish | Tail], Number, List) ->
  Hour = Minutes div 60,
  Fish ! {refresh, Hour, Number, self()},
  receive
    {Fish, ok} -> refreshFish(Minutes, Tail, Number + 1, List ++ [Fish]);
    {Fish, _} -> refreshFish(Minutes, Tail, Number + 1, List) % death
  end.

clearDeadFishLines(FishProcesses, FishLeft, PrinterPid) ->
  DeadFishAmount = length(FishProcesses) - length(FishLeft),
  PrinterPid ! {clearFish, length(FishLeft), DeadFishAmount}.

switchHeater({Temperature, _}, Level) -> {Temperature, Level}.
