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

-define(TYPES_OF_FISH_LINE, 6).
-define(POSSIBLE_COMMANDS_LINE, 7).
-define(SECOND_POSSIBLE_COMMANDS_LINE, 8).

-define(AQUARIUM_STATE_LINE_TEMERATURE, 10).
-define(AQUARIUM_STATE_LINE_HEATER, 11).
-define(AQUARIUM_STATE_LINE_DIRT, 12).
-define(AQUARIUM_STATE_MAX_LEN, 60).
-define(FISH_FIRST_LINE, 14).
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
    clearInfoLine ->
      clearInfoLine(),
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
  screen:writeXY(?INDENT, ?TYPES_OF_FISH_LINE, "\e[1m Types of fish to add:  \e[0m", lightgrey),
  screen:writeXY(?INDENT + 21, ?TYPES_OF_FISH_LINE, " neon, guppy, danio, algaeEater        ", lightgrey),
  screen:writeXY(?INDENT, ?POSSIBLE_COMMANDS_LINE, "\e[1m Possible commands \e[0m", lightgrey),
  screen:writeXY(?INDENT + 19, ?POSSIBLE_COMMANDS_LINE,
    "feed, heaterHigh, heaterNormal, heaterOff", lightgrey),
  screen:writeXY(?INDENT + 19, ?SECOND_POSSIBLE_COMMANDS_LINE,
    "heal, clean, end", lightgrey),
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

  FishString = io_lib:format("<>< ~p, alive time left = ~ph, Hunger = ~p%, Speed = ~pm/s ~p",
    [FishType, RoundedLife, RoundedHunger, RoundedSpeed, Healthy]),
  FishStringBlinking = io_lib:format("<>< ~p, alive time left = ~ph, Hunger = ~p%, Speed = ~pm/s \e[1;5m ~p \e[0m",
    [FishType, RoundedLife, RoundedHunger, RoundedSpeed, Healthy]),
  if
    (RoundedHunger < 60) ->
      if
        (Healthy == healthy) ->
          screen:writeXY(?INDENT, LineNumber, FishString, green);
        true ->
          screen:writeXY(?INDENT, LineNumber, FishStringBlinking, green)
      end;
    (RoundedHunger > 59) and (RoundedHunger < 85) ->
      if
        (Healthy == healthy) ->
          screen:writeXY(?INDENT, LineNumber, FishString, yellow);
        true ->
          screen:writeXY(?INDENT, LineNumber, FishStringBlinking, yellow)
      end;
    (RoundedHunger > 84) ->
      if
        (Healthy == healthy) ->
          screen:writeXY(?INDENT, LineNumber, FishString, red);
        true ->
          screen:writeXY(?INDENT, LineNumber, FishStringBlinking, red)
      end
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
  if
    Temperature < 20 ->
      screen:writeXY(?INDENT, ?AQUARIUM_STATE_LINE_TEMERATURE, io_lib:format("Temperature = ~p°C", [RoundedTemp]), blue);
    (Temperature > 19) and (Temperature < 23) ->
      screen:writeXY(?INDENT, ?AQUARIUM_STATE_LINE_TEMERATURE, io_lib:format("Temperature = ~p°C", [RoundedTemp]), green);
    true ->
      screen:writeXY(?INDENT, ?AQUARIUM_STATE_LINE_TEMERATURE, io_lib:format("Temperature = ~p°C", [RoundedTemp]), red)
  end,
  moveCursorToInputLine(),
  if
    HeaterLevel == normal ->
      screen:writeXY(?INDENT, ?AQUARIUM_STATE_LINE_HEATER, io_lib:format("heater level = ~p", [HeaterLevel]), green);
    HeaterLevel == high ->
      screen:writeXY(?INDENT, ?AQUARIUM_STATE_LINE_HEATER, io_lib:format("heater level = ~p", [HeaterLevel]), red);
    true ->
      screen:writeXY(?INDENT, ?AQUARIUM_STATE_LINE_HEATER, io_lib:format("heater level = ~p", [HeaterLevel]), blue)
  end,
  moveCursorToInputLine(),
  if
    RoundedDirt < 60 ->
      screen:writeXY(?INDENT, ?AQUARIUM_STATE_LINE_DIRT, io_lib:format("Dirt = ~p%", [RoundedDirt]), green);
    (RoundedDirt > 59) and (RoundedDirt < 90) ->
      screen:writeXY(?INDENT, ?AQUARIUM_STATE_LINE_DIRT, io_lib:format("Dirt = ~p%", [RoundedDirt]), yellow);
    true ->
      screen:writeXY(?INDENT, ?AQUARIUM_STATE_LINE_DIRT, io_lib:format("Dirt = ~p%", [RoundedDirt]), red)
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

clearInfoLine() ->
  screen:clearXY(?INDENT, ?INFO_LINE, ?AQUARIUM_STATE_MAX_LEN),
  moveCursorToInputLine().