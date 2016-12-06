-module(screen).

-import(listUtil, [listWithSameElements/2]).

-export([clear_screen/0, writeXY/3, writeXY/4, printBackground/0,
  printLastCommand/1, printTime/2, readLine/0, printFish/3, clearFish/2]).


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

-define(FISH_FIRST_LINE, 7).
-define(FISH_MAX_LEN, 40).

clear_screen() -> io:format("\e[2J").

switchModeString(normal) -> "\e[0m";
switchModeString(bright) -> "\e[1m";
switchModeString(grey) -> "\e[2m";
switchModeString(underline) -> "\e[4m";
switchModeString(blinking) -> "\e[5m";
switchModeString(negative) -> "\e[7m".


escapeXY(X, Y) ->
  "\e[" ++ integer_to_list(Y) ++ ";" ++ integer_to_list(X) ++ "H".


writeXY(X, Y, Message) ->
  writeXY(X, Y, Message, normal).

writeXY(X, Y, Message, Type) ->
  io:format(switchModeString(Type) ++ escapeXY(X, Y) ++ Message ++ switchModeString(normal)).

clearXY(X, Y, Length) ->
  writeXY(X, Y, listWithSameElements(" ", Length)).

moveCursor(X, Y) ->
  io:format(escapeXY(X, Y)).

printBackground() ->
  clear_screen(),
  writeXY(?TITLE_INDENT, ?TITLE_LINE, "##### AQUARIUM #####"),
  writeXY(?INDENT, ?POSSIBLE_COMMANDS_LINE, "Possible commands: feed, newFish, end").

printLastCommand(LastCommand) ->
  clearXY(?INDENT, ?LAST_COMMAND_LINE, ?LAST_COMMAND_LINE_MAX_LEN),
  writeXY(?INDENT, ?LAST_COMMAND_LINE, "Last user command: " ++ LastCommand).

printTime(Hours, Minutes) ->
  clearXY(?CLOCK_X, ?CLOCK_LINE, ?CLOCK_MAX_LEN),
  writeXY(?CLOCK_X, ?CLOCK_LINE, to2Digits(Hours) ++ ":" ++ to2Digits(Minutes)),
  moveCursorToInputLine().

to2Digits(Number) ->
  if
    Number < 10 ->
      "0" ++ integer_to_list(Number);
    true ->
      integer_to_list(Number)
  end.

readLine() ->
  clearXY(?INDENT + ?INPUT_TEXT_START, ?INPUT_LINE, ?INPUT_MAX_LEN),
  moveCursorToInputLine(),
  string:strip(io:get_line("Message to produce: "), right, $\n).

moveCursorToInputLine() ->
  moveCursor(?INDENT, ?INPUT_LINE).

printFish({FishType, LifeTime, _, _}, {Hunger, Speed, AliveTime}, Number) ->
  LineNumber = ?FISH_FIRST_LINE + Number,
  clearXY(?INDENT, LineNumber, ?FISH_MAX_LEN),
  writeXY(?INDENT, LineNumber,
    io_lib:format("<>< ~p, alive time left=~p, Hunger=~p, Speed=~p",
      [FishType, LifeTime - AliveTime, Hunger, Speed])),
  moveCursorToInputLine().

clearFish(_, 0) -> ok;
clearFish(AliveFishAmount, LinesToClear) ->
  LineNumber = ?FISH_FIRST_LINE + AliveFishAmount + LinesToClear - 1,
  clearXY(?INDENT, LineNumber, ?FISH_MAX_LEN),
  moveCursorToInputLine(),
  clearFish(AliveFishAmount, LinesToClear - 1).
