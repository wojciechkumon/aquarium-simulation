-module(aquariumState).

-export([startingAquariumState/0, refreshAquariumState/1]).

% AquariumState = {Temperature, HeaterLevel}
% HeaterLevel = off|normal|high

startingAquariumState() -> {20.0, normal}.

refreshAquariumState({Temperature, off}) ->
  if
    Temperature > 15 ->
      {Temperature - 0.2, off};
    true ->
      {15.0, off}
  end;

refreshAquariumState({Temperature, normal}) ->
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

refreshAquariumState({Temperature, high}) ->
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
