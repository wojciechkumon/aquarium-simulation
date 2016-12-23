-module(printer).

-import(listUtil, [listWithSameElements/2]).
-import(screen, [clearScreen/0, clearXY/3, writeXY/3, moveCursor/2]).

-export([printBackground/0, printLastCommand/1, printTime/2,
  readLine/0, printFish/3, clearFish/2, printAquariumState/1]).


-define(TITLE_LINE, 1).
-define(TITLE_INDENT, 10).
-define(CLOCK_LINE, 1).
-define(CLOCK_X, 34).
-define(CLOCK_MAX_LEN, 5).

-define(INPUT_LINE, 3).
-define(INPUT_MAX_LEN, 20).
-define(INPUT_TEXT_START, 20).

-define(LAST_COMMAND_LINE, 4).
-define(LAST_COMMAND_LINE_MAX_LEN, 40).
-define(INDENT, 3).

-define(POSSIBLE_COMMANDS_LINE, 5).

-define(AQUARIUM_STATE_LINE, 7).
-define(AQUARIUM_STATE_MAX_LEN, 40).
-define(FISH_FIRST_LINE, 8).
-define(FISH_MAX_LEN, 40).


printBackground() ->
  screen:clearScreen(),
  screen:writeXY(?TITLE_INDENT, ?TITLE_LINE, "##### AQUARIUM #####"),
  screen:writeXY(?INDENT, ?POSSIBLE_COMMANDS_LINE, "Possible commands: feed, newFish, end").

printLastCommand(LastCommand) ->
  screen:clearXY(?INDENT, ?LAST_COMMAND_LINE, ?LAST_COMMAND_LINE_MAX_LEN),
  screen:writeXY(?INDENT, ?LAST_COMMAND_LINE, "Last user command: " ++ LastCommand).

printTime(Hours, Minutes) ->
  screen:clearXY(?CLOCK_X, ?CLOCK_LINE, ?CLOCK_MAX_LEN),
  screen:writeXY(?CLOCK_X, ?CLOCK_LINE, to2Digits(Hours) ++ ":" ++ to2Digits(Minutes)),
  moveCursorToInputLine().

to2Digits(Number) ->
  if
    Number < 10 ->
      "0" ++ integer_to_list(Number);
    true ->
      integer_to_list(Number)
  end.

readLine() ->
  screen:clearXY(?INDENT + ?INPUT_TEXT_START, ?INPUT_LINE, ?INPUT_MAX_LEN),
  moveCursorToInputLine(),
  string:strip(io:get_line("Message to produce: "), right, $\n).

moveCursorToInputLine() ->
  screen:moveCursor(?INDENT, ?INPUT_LINE).

printFish({FishType, LifeTime, _, _}, {Hunger, Speed, AliveTime}, Number) ->
  LineNumber = ?FISH_FIRST_LINE + Number,
  screen:clearXY(?INDENT, LineNumber, ?FISH_MAX_LEN),
  screen:writeXY(?INDENT, LineNumber,
    io_lib:format("<>< ~p, alive time left=~p, Hunger=~p, Speed=~p",
      [FishType, LifeTime - AliveTime, Hunger, Speed])),
  moveCursorToInputLine().

clearFish(_, 0) -> ok;
clearFish(AliveFishAmount, LinesToClear) ->
  LineNumber = ?FISH_FIRST_LINE + AliveFishAmount + LinesToClear - 1,
  screen:clearXY(?INDENT, LineNumber, ?FISH_MAX_LEN),
  moveCursorToInputLine(),
  clearFish(AliveFishAmount, LinesToClear - 1).

printAquariumState({Temperature, HeaterLevel}) ->
  screen:clearXY(?INDENT, ?AQUARIUM_STATE_LINE, ?AQUARIUM_STATE_MAX_LEN),
  RoundedTemp = round(Temperature * 10) / 10,
  screen:writeXY(?INDENT, ?AQUARIUM_STATE_LINE,
    io_lib:format("Temperature=~p C, heater level=~p", [RoundedTemp, HeaterLevel])),
  moveCursorToInputLine().