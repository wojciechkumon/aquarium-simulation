-module(aquariumState).

-export([startingAquariumState/0, refreshAquariumState/2, clean/1]).

% AquariumState = {{Temperature, HeaterLevel}, Dirt}
% HeaterLevel = off|normal|high

startingAquariumState() -> {{20.0, normal}, 0}.

refreshAquariumState({TempTuple, Dirt}, FishAmount) ->
  NewTempTuple = refreshTemperature(TempTuple),
  NewDirt = refreshDirt(Dirt, FishAmount),
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


refreshDirt(Dirt, FishAmount) ->
  if
    Dirt < 100 ->
      (0.05 * FishAmount) + Dirt;
    Dirt >= 100 ->
      100
  end.

clean({TempTuple, _}) ->
  {TempTuple, 0}.