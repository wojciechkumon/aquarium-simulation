-module(printer).

-export([startPrinter/0, readLine/0]).


-define(TITLE_LINE, 1).
-define(TITLE_INDENT, 10).
-define(CLOCK_LINE, 1).
-define(INFO_LINE, 2).
-define(CLOCK_X, 34).
-define(CLOCK_MAX_LEN, 5).

-define(INPUT_LINE, 3).
-define(INPUT_MAX_LEN, 20).
-define(INPUT_TEXT_START, 15).

-define(LAST_COMMAND_LINE, 4).
-define(LAST_COMMAND_LINE_MAX_LEN, 40).
-define(INDENT, 3).

-define(TYPES_OF_FISH_LINE, 5).
-define(POSSIBLE_COMMANDS_LINE, 6).
-define(SECOND_POSSIBLE_COMMANDS_LINE, 7).

-define(AQUARIUM_STATE_LINE_TEMERATURE, 9).
-define(AQUARIUM_STATE_LINE_HEATER, 10).
-define(AQUARIUM_STATE_LINE_DIRT, 11).
-define(AQUARIUM_STATE_MAX_LEN, 60).
-define(FISH_FIRST_LINE, 13).
-define(FISH_MAX_LEN, 80).


startPrinter() ->
  printerLoop().

printerLoop() ->
  receive
    {printTime, Hours, Minutes} ->
      printTime(Hours, Minutes),
      printerLoop();
    {printFish, FishConstants, FishVars, Number} ->
      printFish(FishConstants, FishVars, Number),
      printerLoop();
    {clearFish, AliveFishAmount, LinesToClear} ->
      clearFish(AliveFishAmount, LinesToClear),
      printerLoop();
    {printAquariumState, AquariumState} ->
      printAquariumState(AquariumState),
      printerLoop();
    {printInfo, Message} ->
      printInfo(Message),
      printerLoop();
    {printLastCommand, LastCommand} ->
      printLastCommand(LastCommand),
      printerLoop();
    clearScreen ->
      screen:clearScreen(),
      printerLoop();
    printBackground ->
      printBackground(),
      printerLoop();
    printClientBackground ->
      printClientBackground(),
      printerLoop()
  end.

readLine() ->
  screen:clearXY(?INDENT + ?INPUT_TEXT_START, ?INPUT_LINE, ?INPUT_MAX_LEN),
  moveCursorToInputLine(),
  string:strip(io:get_line(""), right, $\n).

printBackground() ->
  screen:clearScreen(),
  screen:writeXY(?TITLE_INDENT, ?TITLE_LINE, "##### AQUARIUM #####", lightblue),
  screen:writeXY(?INDENT, ?TYPES_OF_FISH_LINE, "Types of fish to add:", blue),
  screen:writeXY(?INDENT + 21, ?TYPES_OF_FISH_LINE, " neon, guppy, danio, algaeEater"),
  screen:writeXY(?INDENT, ?POSSIBLE_COMMANDS_LINE,
    "Possible commands: feed, heaterHigh, heaterNormal, heaterOff"),
  screen:writeXY(?INDENT, ?SECOND_POSSIBLE_COMMANDS_LINE,
    "heal, clean, end"),
  screen:writeXY(?INDENT, ?INPUT_LINE, "Enter command: "),
  moveCursorToInputLine().

printLastCommand(LastCommand) ->
  screen:clearXY(?INDENT, ?LAST_COMMAND_LINE, ?LAST_COMMAND_LINE_MAX_LEN),
  screen:writeXY(?INDENT, ?LAST_COMMAND_LINE, "Last user command: " ++ LastCommand),
  moveCursorToInputLine().

printTime(Hours, Minutes) ->
  screen:clearXY(?CLOCK_X, ?CLOCK_LINE, ?CLOCK_MAX_LEN),
  screen:writeXY(?CLOCK_X, ?CLOCK_LINE, to2Digits(Hours) ++ ":" ++ to2Digits(Minutes), darkgrey),
  moveCursorToInputLine().

to2Digits(Number) ->
  if
    Number < 10 ->
      "0" ++ integer_to_list(Number);
    true ->
      integer_to_list(Number)
  end.

moveCursorToInputLine() ->
  screen:moveCursor(?INDENT + ?INPUT_TEXT_START, ?INPUT_LINE).

printFish({FishType, LifeTime, _, _}, {Hunger, Speed, AliveTime, Healthy}, Number) ->
  RoundedHunger = round(Hunger * 10) / 100,
  RoundedSpeed = round(Speed * 10) / 10,
  RoundedLife = round((LifeTime - AliveTime) / 60),
  LineNumber = ?FISH_FIRST_LINE + Number,
  screen:clearXY(?INDENT, LineNumber, ?FISH_MAX_LEN),
  if
    FishType == danio -> screen:writeXY(?INDENT, LineNumber, "<><", danioColor);
    FishType == neon -> screen:writeXY(?INDENT, LineNumber, "<><", neonColor);
    FishType == guppy -> screen:writeXY(?INDENT, LineNumber, "<><", guppyColor);
    true -> screen:writeXY(?INDENT, LineNumber, "<><", algaeEaterColor)
  end,
  FishString = io_lib:format("~p, alive time left=~ph, Hunger=~p%, Speed=~p m/s ~p",
    [FishType, RoundedLife, RoundedHunger, RoundedSpeed, Healthy]),
  if
    Healthy == healthy ->
      screen:writeXY(?INDENT + 4, LineNumber, FishString, green);
    true ->
      screen:writeXY(?INDENT + 4, LineNumber, FishString, ill)
  end,
  if
    RoundedHunger < 60 ->
      screen:writeXY(?INDENT + 4, LineNumber, FishString, green);
    (RoundedHunger > 59) and (RoundedHunger < 85) ->
      screen:writeXY(?INDENT + 4, LineNumber, FishString, yellow);
    true ->
      screen:writeXY(?INDENT + 4, LineNumber, FishString, red)
  end,
  moveCursorToInputLine().

clearFish(_, 0) -> ok;
clearFish(AliveFishAmount, LinesToClear) ->
  LineNumber = ?FISH_FIRST_LINE + AliveFishAmount + LinesToClear - 1,
  screen:clearXY(?INDENT, LineNumber, ?FISH_MAX_LEN),
  moveCursorToInputLine(),
  clearFish(AliveFishAmount, LinesToClear - 1).

printAquariumState({{Temperature, HeaterLevel}, Dirt}) ->
  RoundedTemp = round(Temperature * 10) / 10,
  RoundedDirt = round(Dirt * 10) / 10,
  screen:clearXY(?INDENT, ?AQUARIUM_STATE_LINE_DIRT, ?AQUARIUM_STATE_MAX_LEN),
  screen:clearXY(?INDENT, ?AQUARIUM_STATE_LINE_TEMERATURE, ?AQUARIUM_STATE_MAX_LEN),
  screen:clearXY(?INDENT, ?AQUARIUM_STATE_LINE_HEATER, ?AQUARIUM_STATE_MAX_LEN),
  screen:writeXY(?INDENT, ?AQUARIUM_STATE_LINE_TEMERATURE, io_lib:format("Temperature=~p C", [RoundedTemp]), blue),
  moveCursorToInputLine(),
  screen:writeXY(?INDENT, ?AQUARIUM_STATE_LINE_HEATER, io_lib:format("heater level=~p", [HeaterLevel]), blue),
  moveCursorToInputLine(),
  if
    RoundedDirt < 60 ->
      screen:writeXY(?INDENT, ?AQUARIUM_STATE_LINE_DIRT, io_lib:format("Dirt=~p%", [RoundedDirt]), green);
    (RoundedDirt > 59) and (RoundedDirt < 90) ->
      screen:writeXY(?INDENT, ?AQUARIUM_STATE_LINE_DIRT, io_lib:format("Dirt=~p%", [RoundedDirt]), yellow);
    true ->
      screen:writeXY(?INDENT, ?AQUARIUM_STATE_LINE_DIRT, io_lib:format("Dirt=~p%", [RoundedDirt]), red)
  end,
  moveCursorToInputLine().

printInfo(Message) ->
  screen:clearXY(?INDENT, ?INFO_LINE, ?AQUARIUM_STATE_MAX_LEN),
  screen:writeXY(?INDENT, ?INFO_LINE, Message),
  moveCursorToInputLine().

printClientBackground() ->
  screen:clearScreen(),
  screen:writeXY(?TITLE_INDENT, ?TITLE_LINE, "##### AQUARIUM CLIENT #####", lightblue),
  screen:writeXY(?INDENT, ?POSSIBLE_COMMANDS_LINE,
    "Possible commands: end"),
  screen:writeXY(?INDENT, ?INPUT_LINE, "Enter command: "),
  moveCursorToInputLine().