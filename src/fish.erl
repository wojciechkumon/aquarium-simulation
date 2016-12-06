-module(fish).

-import(screen, [printFish/3]).

-export([startFish/1]).

-define(NOT_HUNGRY, 1000).


% Fish process

startFish(FishType) ->
  fishLoop(getFishConstants(FishType), getFishStartingStats()).

fishLoop(FishConstants, {Hunger, Speed, AliveTime}) ->
  receive
    feed ->
      fishLoop(FishConstants, {?NOT_HUNGRY, Speed, AliveTime});
    {refresh, Number, DispatcherPid} ->
      handleTimeStep(FishConstants, Hunger, Speed, AliveTime, Number, DispatcherPid)
  end.

handleTimeStep(FishConstants, Hunger, Speed, AliveTime, Number, DispatcherPid) ->
  screen:printFish(FishConstants, {Hunger, Speed, AliveTime}, Number),
  NewHunger = changeHunger(Hunger, FishConstants),
  NewAliveTime = AliveTime + 1,
  if
    (NewHunger >= 0) and (NewAliveTime >= 0) ->
      DispatcherPid ! {self(), ok},
      fishLoop(FishConstants, {NewHunger, Speed, NewAliveTime});
    true ->
      DispatcherPid ! {self(), death}
  end.


% {FishType, MaxLifeTime, HungerSpeed, MaxSpeed}
% MaxLifeTime - in timeSteps
% HungerSpeed - per timeStep
getFishConstants(neon) -> {neon, 1440, 5, 10};
getFishConstants(skalar) -> {skalar, 800, 3, 15};
getFishConstants(gupik) -> {gupik, 900, 4, 7}.

% {Hunger, Speed, AliveTime}
getFishStartingStats() -> {?NOT_HUNGRY, 0, 0}.


changeHunger(Hunger, {_, _, HungerSpeed, _}) ->
  Hunger - HungerSpeed.
