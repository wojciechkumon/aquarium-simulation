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
switchModeString(blue) -> "\e[34m";
switchModeString(lightblue) -> "\e[104m";
switchModeString(seablue) -> "\e[38;5;45m";
switchModeString(darkgrey) -> "\e[100m";
switchModeString(pink) -> "\e[95m";
switchModeString(yellow) -> "\e[48;5;225m";
switchModeString(green) -> "\e[42m";
switchModeString(red) -> "\e[41m";
switchModeString(ill) -> "\e[147m";
switchModeString(guppyColor) -> "\e[48;5;208m";
switchModeString(neonColor) -> "\e[48;5;91m";
switchModeString(algaeEaterColor) -> "\e[48;5;64m";
switchModeString(danioColor) -> "\e[48;5;205m".

escapeXY(X, Y) ->
%%  ok.
  "\e[" ++ integer_to_list(Y) ++ ";" ++ integer_to_list(X) ++ "H".

writeXY(X, Y, Message) ->
%%  ok.
  writeXY(X, Y, Message, normal).

writeXY(X, Y, Message, Type) ->
%%  ok.
  io:format(switchModeString(Type) ++ escapeXY(X, Y) ++ Message ++ switchModeString(normal)).

clearXY(X, Y, Length) ->
%%  ok.
  writeXY(X, Y, listWithSameElements(" ", Length)).

moveCursor(X, Y) ->
%%  ok.
  io:format(escapeXY(X, Y)).
