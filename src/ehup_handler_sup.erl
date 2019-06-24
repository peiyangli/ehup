%%%-------------------------------------------------------------------
%%% @author pei
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 十月 2018 14:07
%%%-------------------------------------------------------------------
-module(ehup_handler_sup).
-author("pei").

-behaviour(supervisor).

%% API
-export([start_link/0, handle/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
%%  io:format(">>>>>>start_link ~p", [?FUNCTION_NAME]),
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).


handle(MFO, UdpMsg) ->
  supervisor:start_child(?SERVER, [MFO, UdpMsg]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).
init([]) ->

  SupFlags = {simple_one_for_one, 1000, 3600},
  Cudp_handler_worker = {ehup_handler_worker, {ehup_handler_worker, start_link, []},
    temporary, brutal_kill, worker, [ehup_handler_worker]},

  {ok, {SupFlags, [Cudp_handler_worker]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
