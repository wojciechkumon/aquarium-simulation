-module(fish).

-import(printer, [printFish/3]).

-export([startFish/2]).

-define(NOT_HUNGRY, 1000).


% Fish process

startFish(FishType, PrinterPid) ->
  FishConstants = getFishConstants(FishType),
  fishLoop(FishConstants, getFishStartingStats(FishConstants), PrinterPid).

fishLoop(FishConstants, {Hunger, Speed, AliveTime, Condition}, PrinterPid) ->
  receive
    feed ->
      fishLoop(FishConstants, {?NOT_HUNGRY, Speed, AliveTime, Condition}, PrinterPid);
    {refresh, Hour, Number, DispatcherPid} ->
      NewSpeed = calculateSpeed(FishConstants, Speed, Hour),
      handleTimeStep(FishConstants, Hunger, NewSpeed, AliveTime, Condition, Number, DispatcherPid, PrinterPid);
    heal ->
      NewCondition = tryToHeal(Condition),
      fishLoop(FishConstants, {Hunger, Speed, AliveTime, NewCondition}, PrinterPid)
  end.

handleTimeStep(FishConstants, Hunger, Speed, AliveTime, Condition, Number, DispatcherPid, PrinterPid) ->
  printFish(PrinterPid, FishConstants, Hunger, Speed, AliveTime, Condition, Number),
  NewHunger = changeHunger(Hunger, FishConstants),
  NewCondition = nextStepCondition(Condition),
  NewAliveTime = AliveTime + 1,
  Alive = isAlive(NewHunger, NewAliveTime, NewCondition, FishConstants),
  if
    Alive == true ->
      DispatcherPid ! {self(), ok},
      fishLoop(FishConstants, {NewHunger, Speed, NewAliveTime, NewCondition}, PrinterPid);
    true ->
      DispatcherPid ! {self(), death}
  end.

printFish(PrinterPid, FishConstants, Hunger, Speed, AliveTime, healthy, Number) ->
  PrinterPid ! {printFish, FishConstants, {Hunger, Speed, AliveTime, healthy}, Number};

printFish(PrinterPid, FishConstants, Hunger, Speed, AliveTime, {ill, _}, Number) ->
  PrinterPid ! {printFish, FishConstants, {Hunger, Speed, AliveTime, ill}, Number}.

% {FishType, MaxLifeTime, HungerSpeed, MaxSpeed}
% MaxLifeTime - in timeSteps
% HungerSpeed - per timeStep
getFishConstants(neon) -> {neon, 1440, 5, 10};
getFishConstants(skalar) -> {skalar, 800, 3, 15};
getFishConstants(gupik) -> {gupik, 900, 4, 7}.

% {Hunger, Speed, AliveTime, Healthy}
getFishStartingStats({_, _, _, MaxSpeed}) ->
  {?NOT_HUNGRY, MaxSpeed / 4, 0, healthy}.

changeHunger(Hunger, {_, _, HungerSpeed, _}) ->
  Hunger - HungerSpeed.

isAlive(NewHunger, NewAliveTime, healthy, {_, MaxLifeTime, _, _}) ->
  (NewHunger >= 0) and (NewAliveTime < MaxLifeTime);

isAlive(NewHunger, NewAliveTime, {ill, DeathTimer}, {_, MaxLifeTime, _, _}) ->
  (DeathTimer > 0) and (NewHunger >= 0) and (NewAliveTime < MaxLifeTime).

calculateSpeed({_, _, _, MaxSpeed}, CurrentSpeed, Hour) ->
  NewSpeed = round2ndPlace(CurrentSpeed + newDelta()),
  IsDay = (Hour > 7) and (Hour < 20),
  if
    IsDay ->
      normalizeDaySpeed(NewSpeed, MaxSpeed);
    true ->
      normalizeNightSpeed(NewSpeed, MaxSpeed)
  end.

newDelta() -> rand:normal() / 4.

round2ndPlace(Value) ->
  round(Value * 10) / 10.

normalizeDaySpeed(Speed, MaxSpeed) ->
  normalize(Speed, MaxSpeed / 2, MaxSpeed).

normalizeNightSpeed(Speed, MaxSpeed) ->
  normalize(Speed, 0, MaxSpeed / 2).

normalize(Speed, MinSpeed, MaxSpeed) ->
  if
    Speed > MaxSpeed ->
      round2ndPlace(Speed - abs(newDelta()));
    Speed < MinSpeed ->
      round2ndPlace(MinSpeed + abs(newDelta()));
    true ->
      Speed
  end.

tryToHeal(healthy) ->
  healthy;

tryToHeal({ill, DeathTimer}) ->
  RandomNumber = rand:uniform(10),
  if
    RandomNumber > 3 ->
      healthy;
    true -> {ill, DeathTimer}
  end.

nextStepCondition({ill, DeathTimer}) ->
  {ill, DeathTimer - 1};

nextStepCondition(healthy) ->
  RandomNumber = rand:uniform(1000),
  if
    RandomNumber == 1 ->
      DeathTimer = 60 + rand:uniform(540),
      {ill, DeathTimer};
    true -> healthy
  end.

