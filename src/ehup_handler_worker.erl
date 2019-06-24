%%%-------------------------------------------------------------------
%%% @author pei
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 十月 2018 14:00
%%%-------------------------------------------------------------------
-module(ehup_handler_worker).
-author("pei").

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link(MFO, UdpMsg) ->
  proc_lib:start_link(?MODULE, init, [[self(), MFO, UdpMsg]]).
%%  gen_server:start_link({local, ?SERVER}, ?MODULE, [self(), MFO, UdpMsg], []).

init([Parent, MFO, UdpMsg = {udp, Socket, IP, Port, _}]) ->
%%  io:format("~p~n", [{?FUNCTION_NAME, UdpMsg}]),
  proc_lib:init_ack(Parent, {ok, self()}),
  case handle(MFO, UdpMsg) of
    {reply, Bin}-> gen_udp:send(Socket, IP, Port, Bin);
    {{reply, _}, Bin}-> gen_udp:send(Socket, IP, Port, Bin);
    O->O
  end,
  exit(normal).


handle({M, F}, UdpMsg)->
  M:F(UdpMsg);
handle({M, F, O}, UdpMsg)->
  M:F(UdpMsg, O);
handle(M, UdpMsg)->
  M:init(UdpMsg).
%%  {ok, #state{}}.
