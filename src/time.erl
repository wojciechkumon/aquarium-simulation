-module(time).

-export([startTime/1, startTime/2]).

startTime(Pid) ->
  startTime(Pid, 100).

startTime(Pid, Interval) ->
  {ok, TRef} = timer:send_interval(Interval, Pid, timeStep),
  TRef.