-module(clientRefresher).

-export([startAquariumRefresher/1]).

%% Aquarium state refresher in client

startAquariumRefresher(SocketHandler) ->
  Timer = time:startTime(self(), 1500),
  refresherLoop(SocketHandler, Timer).

refresherLoop(SocketHandler, Timer) ->
  receive
    timeStep ->
      handleRefresh(SocketHandler, Timer),
      refresherLoop(SocketHandler, Timer);
    {stop, Pid} ->
      timer:cancel(Timer),
      Pid ! stopped
  end.

handleRefresh(SocketHandler, Timer) ->
  SocketHandler ! {checkAquariumState, self()},
  receive
    connectionClosed ->
      closeRefresher(Timer);
    error ->
      closeRefresher(Timer);
    {response, Response} ->
      io:format("Aquarium refresher response: ~p~n", [Response])
  end.

closeRefresher(Timer) ->
  io:format("[ClientRefresher] Connection closed~n"),
  timer:cancel(Timer).