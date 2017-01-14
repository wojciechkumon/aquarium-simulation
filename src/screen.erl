-module(screen).

-import(listUtil, [listWithSameElements/2]).

-export([clearScreen/0, writeXY/3, writeXY/4, escapeXY/2, clearXY/3, moveCursor/2]).


clearScreen() -> io:format("\e[2J").

switchModeString(normal) -> "\e[0m";
switchModeString(bright) -> "\e[1m";
switchModeString(grey) -> "\e[2m";
switchModeString(underline) -> "\e[4m";
switchModeString(blinking) -> "\e[5m";
switchModeString(negative) -> "\e[7m";

switchModeString(lightblue) -> "\e[30;48;5;80m";
switchModeString(seablue) -> "\e[38;5;45m";
switchModeString(darkgrey) -> "\e[100m";
switchModeString(lightgrey) -> "\e[30;48;5;254m";
switchModeString(pink) -> "\e[95m";
switchModeString(yellow) -> "\e[30;48;5;220m";
switchModeString(green) -> "\e[30;48;5;76m";
switchModeString(red) -> "\e[30;48;5;196m";
switchModeString(blue) -> "\e[30;48;5;75m".

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
