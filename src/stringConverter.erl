-module(stringConverter).

-export([toString/1, fromString/1]).

%% AquariumState = {{Temperature, HeaterLevel}, Dirt}
%% Fish = [SingleFish,...]
%% SingleFish = {{FishType, MaxLifeTime, HungerSpeed, MaxSpeed}, {Hunger, Speed, AliveTime, Healthy}}

%% example: {{{21.35322534525,heaterOff},52.32423521},[{{neon, 1000, 15, 10}, {214.24, 21.34, 100, ill}},{{guppy, 1200, 25, 20}, {414.74, 13.12, 220, healthy}}]}

toString({AquariumState, Fish}) ->
  aquariumStateToString(AquariumState) ++ "|" ++ fishToString(Fish).

aquariumStateToString({{Temperature, HeaterLevel}, Dirt}) ->
  floatToString(Temperature) ++ "$" ++ atom_to_list(HeaterLevel) ++ "#" ++ floatToString(Dirt).

fishToString([{{FishType, MaxLifeTime, HungerSpeed, MaxSpeed}, {Hunger, Speed, AliveTime, Healthy}}]) ->
  atom_to_list(FishType) ++ "&" ++ intToString(MaxLifeTime) ++ "&" ++ intToString(HungerSpeed)
    ++ "&" ++ intToString(MaxSpeed)
    ++ "@" ++ floatToString(Hunger) ++ "&" ++ floatToString(Speed) ++ "&"
    ++ intToString(AliveTime) ++ "&" ++ atom_to_list(Healthy);

fishToString([X | Tail]) ->
  fishToString([X]) ++ "*" ++ fishToString(Tail).


fromString(String) ->
  [AquariumStateString | FishStringList] = string:tokens(String, "|"),
  if
    length(FishStringList) == 0 ->
      {fromAquariumStateString(AquariumStateString), []};
    true ->
      {fromAquariumStateString(AquariumStateString), fromFishListString(lists:flatten(FishStringList))}
  end.

fromAquariumStateString(AquariumStateString) ->
  List = string:tokens(AquariumStateString, "#"),
  [TempHeaterLevel | Dirt] = List,
  TempHeaterLevelList = string:tokens(TempHeaterLevel, "$"),
  [Temperature | HeaterLevel] = TempHeaterLevelList,
  {TemperatureNumber, _} = string:to_float(Temperature),
  HeaterLevelAtom = list_to_atom(lists:flatten(HeaterLevel)),
  {DirtNumber, _} = string:to_float(lists:flatten(Dirt)),
  {{TemperatureNumber, HeaterLevelAtom}, DirtNumber}.

fromFishListString(FishString) ->
  FishList = string:tokens(FishString, "*"),
  lists:map(fun(SingleFishString) -> fromSingleFishString(SingleFishString) end, FishList).

fromSingleFishString(FishString) ->
  [ConstFish | VariableFish] = string:tokens(FishString, "@"),
  [FishTypeString, MaxLifeTimeString, HungerSpeedString | MaxSpeedString] = string:tokens(ConstFish, "&"),
  [HungerString, SpeedString, AliveTimeString | HealthyString] = string:tokens(lists:flatten(VariableFish), "&"),
  FishType = list_to_atom(FishTypeString),
  {MaxLifeTime, _} = string:to_integer(MaxLifeTimeString),
  {HungerSpeed, _} = string:to_integer(HungerSpeedString),
  {MaxSpeed, _} = string:to_integer(lists:flatten(MaxSpeedString)),
  {Hunger, _} = string:to_float(HungerString),
  {Speed, _} = string:to_float(SpeedString),
  {AliveTime, _} = string:to_integer(AliveTimeString),
  Healthy = list_to_atom(lists:flatten(HealthyString)),
  {{FishType, MaxLifeTime, HungerSpeed, MaxSpeed}, {Hunger, Speed, AliveTime, Healthy}}.

floatToString(Float) ->
  lists:flatten(io_lib:format("~p", [Float])).

intToString(Integer) ->
  lists:flatten(io_lib:format("~p", [Integer])).