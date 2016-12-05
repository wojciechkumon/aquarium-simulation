-module(listUtil).

-export([listWithSameElements/2]).

% example: listWithSameElements("b", 4) = "bbbb"
listWithSameElements(X, N) ->
  lists:flatten(lists:duplicate(N, X)).
