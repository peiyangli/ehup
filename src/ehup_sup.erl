%%%-------------------------------------------------------------------
%% @doc ehup top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ehup_sup).

-behaviour(supervisor).

%% API
-export([start_link/1, to_sup_id/1]).

%% Supervisor callbacks
-export([init/1]).

-export([start_hup/2, start_listening/2]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Args]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}




all_servers(Servers)->
    [ {to_sup_id(Srv), {ehup_listener_sup, start_link, [to_sup_id(Srv)]},
        permanent, 2000, supervisor, [ehup_listener_sup]} || Srv <- Servers].

init([Args]) ->
    SupFlags = {one_for_one, 4, 3600}, % {one_for_all, 0, 1}
    Cudp_handler_sup = {ehup_handler_sup, {ehup_handler_sup, start_link, []},
        permanent, 2000, supervisor, [ehup_handler_sup]},
%%    Cudp_listener_sup = {ehup_listener_sup, {ehup_listener_sup, start_link, []},
%%        permanent, 2000, supervisor, [ehup_listener_sup]},
    Sups = [Cudp_handler_sup|all_servers(Args)],
    {ok, { SupFlags, Sups} }.

%%child_spec(Ref, NumAcceptors, Transport, TransOpts, Protocol, ProtoOpts)
%%    when is_integer(NumAcceptors) andalso is_atom(Transport)
%%    andalso is_atom(Protocol) ->
%%    {{ehup_listener_sup, Ref}, {ehup_listener_sup, start_link, [
%%        Ref, NumAcceptors, Transport, TransOpts, Protocol, ProtoOpts
%%    ]}, permanent, infinity, supervisor, [ranch_listener_sup]}.




to_sup_id(S)->
    list_to_atom(ehup_utils:format("~p_sup", [S])).

to_child_id(S)->
    list_to_atom(ehup_utils:format("~p_child", [S])).

% Args = {Port, MFO = {M, F, O}}
start_listening(Srv1, Args)->
    io:format("~p~n", [{self(), ?FUNCTION_NAME, Args, Srv1}]),
%%    Srv1 = to_sup_id(Srv0),
    {ok, _} = supervisor:start_child(?SERVER, {Srv1, {ehup_listener_sup, start_link, [Srv1]},
        permanent, 2000, supervisor, [ehup_listener_sup]}),
    supervisor:start_child(Srv1, [to_child_id(Srv1), Args]).





eg_port_router()->
    [{port, {8888, [{ip, {127,0,0,1}}]}}
        , {router, [
        hup_handlers
        ,hup_test
        ,{hup_test, "/p"}
        ,{"/test", {hup_test, echo_query}}
        ,{"/testa", {hup_test, echo_query, 1}}
    ]}].

start_hup(Srv, {Port, Routers})->
    Sid = to_sup_id(Srv),
    start_listening(Sid, {Port, {ehup_hup, init, Routers}}).
%%    {ok, _} = supervisor:start_child(?SERVER, {Sid, {ehup_listener_sup, start_link, [Sid]},
%%        permanent, 2000, supervisor, [ehup_listener_sup]}),
%%    start_listening(Sid, {Port, {ehup_hup, init, Routers}}).
%%====================================================================
%% Internal functions
%%====================================================================
