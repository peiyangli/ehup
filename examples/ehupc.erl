%%%-------------------------------------------------------------------
%%% @author pei
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 十月 2018 14:30
%%%-------------------------------------------------------------------
-module(ehupc).
-author("pei").

%% API
-export([query/1, query/3]).

% ehupc:query({"211.149.133.55", 3455}, <<"{\"method\":\"allocate/chat\",\"args\":{\"count\":10,\"loc\":{\"lat\":104,\"lon\":30.1}}}">>, 2000).
% ehupc:query({"10.10.1.102", 3553}, <<"{\"method\":\"allocate/chat\",\"args\":{\"ver\":65537,\"count\":10,\"loc\":{\"lat\":104,\"lon\":30.1}}}">>, 2000).


%%ehupc:query({"localhost", 1111}, <<"GET /">>, 3000).

query(Bin) ->
  query({"localhost", 3455}, Bin, 2000).

query({Host, Port}, Bin, Timeout) ->
  {ok, Socket} = gen_udp:open(0, [binary]),
  try
    ok = gen_udp:send(Socket, Host, Port, Bin),
    receive
      {udp, Socket, _, _, Data} ->
        Data
    after Timeout -> <<>>
    end
  after
    gen_udp:close(Socket)
  end.