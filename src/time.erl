-module(time).

-export([startTime/1]).

startTime(DispatcherPid) ->
  timer:send_interval(100, DispatcherPid, timeStep).
