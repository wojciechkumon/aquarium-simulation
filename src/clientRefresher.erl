-module(clientRefresher).

-export([startAquariumRefresher/2]).

-define(ASK_SERVER_RATE, 500).

%% Aquarium state refresher in client

startAquariumRefresher(SocketHandler, PrinterPid) ->
  Timer = time:startTime(self(), ?ASK_SERVER_RATE),
  refresherLoop(SocketHandler, {Timer, PrinterPid}).

refresherLoop(SocketHandler, {Timer, PrinterPid}) ->
  receive
    timeStep ->
      handleRefresh(SocketHandler, {Timer, PrinterPid}),
      refresherLoop(SocketHandler, {Timer, PrinterPid});
    {stop, Pid} ->
      timer:cancel(Timer),
      Pid ! stopped
  end.

handleRefresh(SocketHandler, {Timer, PrinterPid}) ->
  SocketHandler ! {checkAquariumState, self()},
  receive
    {response, connectionClosed} ->
      closeRefresher(Timer, PrinterPid);
    {response, error} ->
      closeRefresher(Timer, PrinterPid);
    {response, Response} ->
      Aquarium = stringConverter:fromString(Response),
      refreshView(Aquarium, PrinterPid)
  end.

closeRefresher(Timer, PrinterPid) ->
  PrinterPid ! {printInfo, "Connection lost, use \"end\" to finish"},
  timer:cancel(Timer).

refreshView({AquariumState, Fish}, PrinterPid) ->
  PrinterPid ! {printAquariumState, AquariumState},
  PrinterPid ! {clearFish, length(Fish), 3},
  printFish(PrinterPid, Fish).

printFish(PrinterPid, Fish) ->
  printFish(PrinterPid, Fish, 0).

printFish(_, [], _) -> ok;

printFish(PrinterPid, [Fish | Tail], Number) ->
  printSingleFish(PrinterPid, Fish, Number),
  printFish(PrinterPid, Tail, Number + 1).

printSingleFish(PrinterPid, {FishConstants, FishVars}, Number) ->
  PrinterPid ! {printFish, FishConstants, setCorrectHealthyOrIll(FishVars), Number}.

setCorrectHealthyOrIll({Hunger, Speed, AliveTime, healthy}) ->
  {Hunger, Speed, AliveTime, healthy};

setCorrectHealthyOrIll({Hunger, Speed, AliveTime, {ill, _}}) ->
  {Hunger, Speed, AliveTime, ill}.