-module(fish).

-import(screen, [printFish/3]).

-export([startFish/2]).

-define(NOT_HUNGRY, 1000).


% Fish process

startFish(FishType, DispatcherPid) ->
  fishLoop(getFishConstants(FishType), getFishStartingStats()).

fishLoop(FishConstants, {Hunger, Speed, AliveTime}) ->
  receive
    feed ->
      fishLoop(FishConstants, {?NOT_HUNGRY, Speed, AliveTime});
    {refresh, Number} ->
      screen:printFish(FishConstants, {Hunger, Speed, AliveTime}, Number),
      fishLoop(FishConstants, {changeHunger(Hunger, FishConstants), Speed, AliveTime})
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
