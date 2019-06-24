%%%-------------------------------------------------------------------
%%% @author pei
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. 五月 2019 11:05
%%%-------------------------------------------------------------------
-module(ehup_hup_test).
-author("pei").

%% API
-export([]).

-compile(export_all).

-include("../include/ehup.hrl").

%% see golang hup
test(Port)->
  ehup_sup:start_hup(ehup_listener, {Port, #{
    <<"/">> =>
    fun(C0)->
      io:format("Query: ~p~n", [C0]),
      C1 = ehup_hup:multipart(C0),
      #context{request = #request{multipart = MultiPart}} = C1,
      io:format("MultiPart: ~p~n", [MultiPart]),
      ehup_hup:response(C1, {200, [], <<"ok">>})
    end,
    <<"/crash">> => fun(C)-> x=n end
  }}).

test(Srv, Port)->
  ehup_sup:start_hup(Srv, {Port, #{
    <<"/">> => fun(C)-> ehup_hup:response(C, {200, [], <<"ok">>}) end,
    <<"/crash">> => fun(C)-> x=n end
  }}).