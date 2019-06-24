%%%-------------------------------------------------------------------
%% @doc cudp public API
%% @end
%%%-------------------------------------------------------------------

-module(ehup_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================
%%//muti server
start(_StartType, _StartArgs) ->
    case application:get_env(ehup, servers) of
        {ok, Servers}->
            ehup_sup:start_link(Servers);
        _->
            ehup_sup:start_link([])
    end.


%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================