-module(aquariumClient).

-export([start/0, start/1, start/2]).

-define(TCP_OPTIONS, [{active, false}, {packet, 2}]).


start() ->
  start(aquariumServer:defaultPort()).

start(Port) ->
  start(aquariumServer:defaultHost(), Port).

start(Host, Port) ->
  {ok, Socket} = gen_tcp:connect(Host, Port, aquariumServer:tcpOptions()),
  loop(Socket).

loop(Sock) ->
  Message = "y",
  case gen_tcp:send(Sock, [Message]) of
    {error, timeout} ->
      io:format("Send timeout, closing!~n", []),
      handle_send_timeout(),
      gen_tcp:close(Sock);
    {error, OtherSendError} ->
      io:format("Some other error on socket (~p), closing", [OtherSendError]),
      gen_tcp:close(Sock);
    ok ->
      io:format("OK"),
      gen_tcp:send(Sock, ["finish"]),
      gen_tcp:close(Sock)
%%      loop(Sock)
  end.


handle_send_timeout() ->
  io:format("Error while sending message").