-module(screen).

-export([clearScreen/0, writeXY/3, writeXY/4, escapeXY/2, clearXY/3, moveCursor/2]).


clearScreen() -> io:format("\e[2J").

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
