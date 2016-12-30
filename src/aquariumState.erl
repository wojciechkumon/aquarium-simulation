-module(aquariumState).

-export([startingAquariumState/0, refreshAquariumState/3, clean/1]).

% AquariumState = {{Temperature, HeaterLevel}, Dirt}
% HeaterLevel = off|normal|high

startingAquariumState() -> {{20.0, normal}, 0}.

refreshAquariumState({TempTuple, Dirt}, FishAmount, AlgaeEaters) ->
  NewTempTuple = refreshTemperature(TempTuple),
  NewDirt = refreshDirt(Dirt, FishAmount, AlgaeEaters),
  {NewTempTuple, NewDirt}.

refreshTemperature({Temperature, off}) ->
  if
    Temperature > 15 ->
      {Temperature - 0.2, off};
    true ->
      {15.0, off}
  end;

refreshTemperature({Temperature, normal}) ->
  if
    Temperature < 22 ->
      {Temperature + 0.01, normal};
    Temperature > 32 ->
      {Temperature - 0.01, normal};
    Temperature > 25 ->
      {Temperature - 0.005, normal};
    true ->
      {Temperature, normal}
  end;

refreshTemperature({Temperature, high}) ->
  if
    Temperature < 22 ->
      {Temperature + 0.02, high};
    Temperature < 32 ->
      {Temperature + 0.01, high};
    Temperature < 40 ->
      {Temperature - 0.004, high};
    true ->
      {Temperature, high}
  end.

refreshDirt(Dirt, FishAmount, AlgaeEaters) ->
  DirtToAdd = (0.05 * (FishAmount - AlgaeEaters)),
  DirtToRemove = 0.1 * AlgaeEaters,
  RandomDirt = (rand:uniform(11) - 1) / 100,
  NewDirt = Dirt + DirtToAdd - DirtToRemove + RandomDirt,
  if
    NewDirt >= 100 -> 100;
    NewDirt =< 0 -> 0;
    true -> NewDirt
  end.

clean({TempTuple, _}) ->
  {TempTuple, 0}.
