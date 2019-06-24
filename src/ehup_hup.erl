%%%-------------------------------------------------------------------
%%% @author pei
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. 五月 2019 10:21
%%%-------------------------------------------------------------------
-module(ehup_hup).
-author("pei").

%% API
-export([multipart/1, get_multipart/1]).
-export([init/2]).
-export([next/1]).
-export([response/2, send/2, response/1]).
-export([encode/3, parse_query/1]).
-export([get_routers/1]).
-export([set_header/2, set_status/3]).

%%-compile(export_all).

-include("../include/ehup.hrl").

%%
%%start(Srv, {Port, Routers})->
%%  ehup_sup:start_listening(Srv, {Port, {?MODULE, init, Routers}}).

%%//{udp, Socket, IP, Port, Data}
init({udp, Socket, IP, Port, ReqBin}, Routers)->
  case parse(ReqBin) of
    {ok, Req = #request{path = Path}}->
      case maps:find(Path, Routers) of
        {ok, Handlers}-> next(#context{request = Req, net=#net{sock = Socket, ip = IP, port= Port}, handlers = Handlers});
        _->
          gen_udp:send(Socket, IP, Port, encode(404, [], <<"not found">>))
      end;
    Err->
      io:format("error: ~p~n", [Err]),
      Err
  end.



send(#net{sock = S, ip = Ip, port = Port}, {Status, Headers, Body})->
  gen_udp:send(S, Ip, Port, encode(Status, Headers, Body));
send(#net{sock = S, ip = Ip, port = Port}, #response{status=Status, headers=Headers, body=Body})->
  gen_udp:send(S, Ip, Port, encode(Status, Headers, Body));
send({S, Ip, Port}, IoList)->
  gen_udp:send(S, Ip, Port, IoList).

%%response(#net{sock = S, ip = Ip, port = Port}, #response{status=Status, headers=Headers, body=Body})->
%%  gen_udp:send(S, Ip, Port, encode(Status, Headers, Body));
%%response(#net{sock = S, ip = Ip, port = Port}, {Status, Headers, Body})->
%%  gen_udp:send(S, Ip, Port, encode(Status, Headers, Body)).

response(#context{net=Net, response = #response{headers = Heads}}, {Status, Headers, Body})->
  send(Net, {Status, headers_merge(Heads, Headers), Body});
response(#context{net=Net, response = #response{headers = Heads}}, Body) when is_binary(Body)->
  send(Net, {200, Heads, Body}).
response(#context{net=Net, response = Resp})->
  send(Net, Resp).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%// abort, next
%% {ok|abort, Context:#context}
next(Ctx = #context{handlers = []})->
  {ok, Ctx};
next(Ctx0 = #context{handlers = [Handler|Left]})->
  Ctx1 = Ctx0#context{handlers = Left},
  case call_crash_500(Handler, Ctx1) of
    {next, Ctx}->next(Ctx);
    {abort, Ctx}->{abort, Ctx};
    _->{abort, Ctx1}
  end;
next(Ctx = #context{handlers = Handler})->
  call_crash_500(Handler, Ctx#context{handlers = []}).


call_crash_500(F, Ctx)->
  try
    call(F, Ctx)
  catch
    _E:_W->
      response(Ctx, {500, [], <<"server internal error">>}),
      {abort, Ctx}
  end.

% abort, {next, NewCtx}, {abort, NewCtx}
call({M,F,A}, Ctx)->
  M:F(Ctx,A);
call({M,F}, Ctx) when is_atom(M)->
  M:F(Ctx);
call({F,A}, Ctx) when is_function(F, 2)->
  F(Ctx,A);
call(F, Ctx) when is_function(F, 1)->
  F(Ctx).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Header: {K,V}, #{}, [{K,V}]
set_header(Header, C=#context{response = Resp = #response{headers = Headers}})->
  C#context{response = Resp#response{headers = headers_merge(Headers, Header)}}.

set_status(K, V, C=#context{status = Map})->
  C#context{status = Map#{K=>V}}.


%%  case request(ReqBin) of
%%    {ok, Req}->
%%      method(Req#{?ip => IP});
%%    Err->
%%      Bin = jiffy:encode(#{code => ?Err_Param, error => <<"args">>}),
%%      {{reply, Err}, Bin}
%%  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%querys(#context{query = Q})when is_binary(Q)->
%%  parse_query(Q).
%%
%%query(K, #context{query = Q})->
%%  case parse_query(Q) of
%%    {ok, Qs}->
%%      proplists:get_value(K, Qs);
%%    _-> undefined
%%  end;
%%query(K, Qs) when is_list(Qs)->
%%  proplists:get_value(K, Qs).
%%
%%query(K, #context{query = Q}, Def)->
%%  case parse_query(Q) of
%%    {ok, Qs}->
%%      proplists:get_value(K, Qs, Def);
%%    _-> Def
%%  end;
%%query(K, Qs, Def) when is_list(Qs)->
%%  proplists:get_value(K, Qs, Def).
%%
%%querys(K, #context{query = Q})->
%%  case parse_query(Q) of
%%    {ok, Qs}->
%%      proplists:get_all_values(K, Qs);
%%    _-> []
%%  end;
%%querys(K, Qs) when is_list(Qs)->
%%  proplists:get_all_values(K, Qs).
%%
%%raw_query(#context{query = Q})->
%%  Q.
%%
%%
%%method(#context{method = V})->V.
%%path(#context{path = V})->V.
%%headers(#context{headers = V})->V.
%%body(#context{body = V})->V.
%%ip(#context{ip = V})->V.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% {ok, [{name, val}]}
parse_query(Q)when is_binary(Q)->
  case parse_query_key(Q, []) of
    {ok, Acc}->{ok, lists:reverse(Acc)};
    Err->Err
  end;
parse_query(#context{request = #request{query = Query}})->
  parse_query(Query);
parse_query(_)->{error, query_not_binary}.

parse_query_key(<<>>, Acc)->
  {ok, Acc};
parse_query_key(Q, Acc)->
  case binary:split(Q, [<<$=>>], []) of
    [A]->parse_query_val(<<>>, A, Acc);
    [A, B]->parse_query_val(B, A, Acc)
  end.

parse_query_val(<<>>, Key, Acc)->
  {error, {no_val, Key, Acc}};
parse_query_val(Q, Key, Acc)->
  case binary:split(Q, [<<$&>>], []) of
    [A]->{ok, [{Key, A}|Acc]};
    [A,B]->parse_query_key(B, [{Key, A}|Acc])
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_query_map(Q)->
  parse_query_key(Q, #{}).

parse_query_map(<<>>, Acc)->
  {ok, Acc};
parse_query_map(Q, Acc)->
  case binary:split(Q, [<<$=>>], []) of
    [A]->parse_query_map_val(<<>>, A, Acc);
    [A, B]->parse_query_map_val(B, A, Acc)
  end.

parse_query_map_val(<<>>, Key, Acc)->
  {error, {no_val, Key, Acc}};
parse_query_map_val(Q, Key, Acc)->
  case binary:split(Q, [<<$&>>], []) of
    [A]->{ok, Acc#{Key => A}};
    [A,B]->parse_query_map(B, Acc#{Key => A})
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
set_path(RawQuery, Req)->
  case binary:split(RawQuery, [<<$?>>], []) of
    [A]->Req#request{path = A};
    [A, B]->
      Req#request{path = A, query = B}
  end.

trim(<<>>)-><<>>;
trim(<<$\s, Rest/binary>>)->trim(Rest);
trim(<<$\t, Rest/binary>>)->trim(Rest);
%%trim(<<$\n, Rest/binary>>)->trim(Rest);
%%trim(<<$\r, Rest/binary>>)->trim(Rest);
trim(Rest)->Rest.


trim_line(<<>>)-><<>>;
trim_line(<<$\n, Rest/binary>>)->trim(Rest);
trim_line(<<$\r, Rest/binary>>)->trim(Rest);
trim_line(Rest)->Rest.

trim_all(<<>>)-><<>>;
trim_all(<<$\s, Rest/binary>>)->trim(Rest);
trim_all(<<$\t, Rest/binary>>)->trim(Rest);
trim_all(<<$\n, Rest/binary>>)->trim(Rest);
trim_all(<<$\r, Rest/binary>>)->trim(Rest);
trim_all(Rest)->Rest.

parse(Bin)->
  case binary:split(Bin, [<<$\n,$\n>>], []) of
    [A]->parse_method(trim(A), #request{});
    [A, B]->  parse_method(trim(A), #request{body = B})
  end.

parse_method(<<>>, _Req)->
  {error, no_method};
parse_method(Bin, Req)->
  case binary:split(Bin, [<<$ >>,<<$\t>>], []) of %trim
    [A]->{ok, Req#request{method = A}};
    [A, B]-> parse_path(trim(B), Req#request{method = A})
  end.

parse_path(<<>>, _Req)->
  {error, no_path};
parse_path(Bin, Req)->
  case binary:split(Bin, [<<$\n>>], []) of
    [A]-> {ok, set_path(A, Req)};
    [A, B]-> parse_header(B, set_path(A, Req))
  end.

parse_header(<<>>, Req)->
  {ok, Req};
parse_header(<<$\n, Rest/binary>>, Req)->
  {ok, Req#request{body = Rest}};
parse_header(Bin, Req)->
  parse_header_key(trim(Bin), Req).


parse_header_key(<<>>, Req)->
  {ok, Req};
parse_header_key(Bin, Req=#request{headers = Headers})->
  case binary:split(Bin, [<<$:>>], []) of
    [A]->{ok, Req#request{headers = Headers#{A=> <<>>}}};
    [A, B]-> parse_header_val(trim(B), A, Req)
  end.

parse_header_val(<<>>, Key, Req=#request{headers = Headers})->
  {ok, Req#request{headers = Headers#{Key=> <<>>}}};
parse_header_val(Bin, Key, Req=#request{headers = Headers})->
  case binary:split(Bin, [<<$\n>>], []) of
    [A]->{ok, Req#request{headers = Headers#{Key => A}}};
    [A, B]-> parse_header(B, Req#request{headers = Headers#{Key => A}})
  end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% return iolist
encode(Status, Headers, Body) when is_integer(Status)->
  [?SERVER_VER, value_to_list(Status), status_string(Status),header_to_list(headers_merge(Headers, {?Body_Length, body_length(Body)})), $\n, $\n, Body].

body_length(Body)->
  iolist_size(Body).


status_string(200)->" OK";
status_string(500)->" InternalError";
status_string(400)->" BadRequest";
status_string(401)->" Unauthorized";
status_string(_)->"".

value_to_list(V)when is_integer(V)->
  integer_to_list(V);
value_to_list(V)when is_float(V)->
  float_to_list(V);
value_to_list(V)when is_atom(V)->
  atom_to_list(V);
value_to_list(V)->
  V.

header_to_list(Headers)when is_list(Headers)->
  header_to_list(Headers, []);
header_to_list(Headers) when is_map(Headers)->
  maps:fold(
    fun(K, V, Acc)->
      [Acc, $\n, value_to_list(K), ": ", value_to_list(V)]
    end, [], Headers);
header_to_list(_)->
  <<>>.

header_to_list([], Acc)->
  Acc;
header_to_list([{K, V}|Headers], Acc)->
  header_to_list(Headers, [Acc, $\n, value_to_list(K), ": ", value_to_list(V)]).

headers_merge(H1, H2) when is_map(H1)->
  if
    is_map(H2) -> ehup_utils:merge(H1, H2);
    is_list(H2)-> lists:foldl(fun({K,V},AI)-> AI#{K=>V} end, H1, H2);
    true ->
      case H2 of
        {K, V}-> H1#{K=>V};
        _->H1
      end
  end;
headers_merge(H1, H2) when is_map(H2)->
  if
    is_list(H1)-> lists:foldl(fun({K,V},AI)-> AI#{K=>V} end, H2, H1);
    true ->
      case H1 of
        {K, V}-> H2#{K=>V};
        _->H2
      end
  end;
headers_merge(H1, KVs) when is_list(H1)->
  R = lists:foldl(fun({K,V},AI)-> AI#{K=>V} end, #{}, H1),
  case KVs of
    {K, V} -> R#{K => V};
    [_|_] ->lists:foldl(fun({K,V},AI)-> AI#{K=>V} end, R, KVs);
    _->R
  end;
headers_merge(KVs, H1) when is_list(H1)->
  case KVs of
    {K, V} -> lists:foldl(fun({KI,VI},AI)-> AI#{KI=>VI} end, #{K => V}, H1);
    [_|_] ->
      R = lists:foldl(fun({KI,VI},AI)-> AI#{KI=>VI} end, #{}, KVs),
      lists:foldl(fun({KI,VI},AI)-> AI#{KI=>VI} end, R, H1);
    _->lists:foldl(fun({KI,VI},AI)-> AI#{KI=>VI} end, #{}, H1)
  end.

%%headers_merge(H1,H2) when is_list(H1)->
%%  if
%%    is_list(H2)->
%%      H1 ++ H2;
%%    is_map(H2)->
%%      H1++
%%      maps:fold(
%%        fun(K, V, AI)->
%%          [{K, V}|AI]
%%        end, [], H2);
%%    true->H1
%%  end;
%%headers_merge(H1,H2) when is_list(H2)->
%%  if
%%    is_list(H1)->
%%      H1 ++ H2;
%%    is_map(H1)->
%%      maps:fold(
%%        fun(K, V, AI)->
%%          [{K, V}|AI]
%%        end, [], H1) ++ H2;
%%    true->H1
%%  end;
%%headers_merge(H1,H2) when is_map(H1) andalso is_map(H2)->
%%  maps:fold(
%%    fun(K, V, AI)->
%%      [{K, V}|AI]
%%    end, [], H1)
%%  ++
%%  maps:fold(
%%    fun(K, V, AI)->
%%      [{K, V}|AI]
%%    end, [], H2);
%%headers_merge(_,_)->[].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Routers = #{} |
%% [Module|{Module, Args}, {Module, Func, Args}, {Path, MFA}]
get_routers(Routers) when is_map(Routers)->
  {ok, Routers};
get_routers(Routers) when is_list(Routers)->
  get_routers(Routers, #{}).

get_routers([], Acc)->
  {ok, Acc};
get_routers([Module|Left], Acc) when is_atom(Module)-> %% module export init(AccIn)->{ok, AccOut}
  case Module:init(Acc) of
    {ok, Acc1}-> get_routers(Left, Acc1);
    _->get_routers(Left, Acc)
  end;
get_routers([{Module, Args}|Left], Acc) when is_atom(Module)-> %% module export init(AccIn)->{ok, AccOut}
  case Module:init(Acc, Args) of
    {ok, Acc1}-> get_routers(Left, Acc1);
    _->get_routers(Left, Acc)
  end;
get_routers([{Module,Func,Args}|Left], Acc) when is_atom(Module)-> %% module export init(AccIn)->{ok, AccOut}
  case Module:Func(Acc, Args) of
    {ok, Acc1}-> get_routers(Left, Acc1);
    _->get_routers(Left, Acc)
  end;
get_routers([{Path, MFA}|Left], Acc)->
  get_routers(Left, Acc#{dg_bin:from(Path) => MFA}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
-define(length, <<"length">>).
-define(name, <<"name">>).
-define(filename, <<"filename">>).
-define(content, <<"content">>).
-define(boundary, <<"boundary">>).



multipart(Ctx = #context{request = Req = #request{headers = Headers, body = Body, multipart=Multipart}}) when is_atom(Multipart)->
  case maps:get(?Content_Type, Headers, undefined) of
    undefined-> Ctx#context{request = Req#request{multipart = {error, undefined}}};
    ContentType->
      case parse_body(ContentType, Body) of
        {ok, Val}-> Ctx#context{request = Req#request{multipart = {ok, Val}}};
        Err->Ctx#context{request = Req#request{multipart = {error, Err}}}
      end
  end;
multipart(Ctx)->
  Ctx.

get_multipart(#context{request =#request{multipart=Multipart}})->Multipart.

parse_body(<<"multipart/form-data;", BoundaryBin/binary>>, Body)->
  case form_data_args(trim(BoundaryBin)) of
    Args->
      case proplists:get_value(?boundary, Args, undefined) of
        Boundary when is_binary(Boundary) andalso byte_size(Boundary) > 5->
          {ok, form_data_parse(Boundary, Body)};
        Boundary->
          {error, {boundary, Boundary}}
      end
  end;
parse_body(<<"multipart/compact;", _/binary>>, Body)->
  compact_parse(trim_all(Body));
parse_body(CT,_)->
  {error, {not_supported, CT}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


parse_multipart_header(<<>>, MP)->
  {ok, MP};
parse_multipart_header(Bin, MP)->
  parse_multipart_header_key(trim(Bin), MP).


parse_multipart_header_key(<<>>, MP)->
  {ok, MP};
parse_multipart_header_key(Bin, MP=#multipart{headers = Headers})->
  case binary:split(Bin, [<<$:>>], []) of
    [A]->{ok, MP#multipart{headers = Headers#{A=> <<>>}}};
    [A, B]-> parse_multipart_header_val(trim(B), A, MP)
  end.

parse_multipart_header_val(<<>>, Key, MP=#multipart{headers = Headers})->
  {ok, MP#multipart{headers = Headers#{Key=> <<>>}}};
parse_multipart_header_val(Bin, Key, MP=#multipart{headers = Headers})->
  case binary:split(Bin, [<<$\n>>], []) of
    [A]->{ok, MP#multipart{headers = Headers#{Key => A}}};
    [A, B]-> parse_multipart_header(trim_line(B), MP#multipart{headers = Headers#{Key => A}})
  end.

form_data_args(<<>>)->[];
form_data_args(Boundary)->
  lists:foldr(
    fun(KV, AI)->
      case binary:split(trim(KV), <<"=">>, []) of
        [K] -> [{trim(K), undefined}|AI];
        [K, V] -> [{trim(K), V}|AI]
      end
    end, [], binary:split(Boundary, <<";">>, [global])).


% [{name, Multipart}]
form_data_parse(Boundary, Body)->
  lists:foldr(
    fun(<<>>, AI)-> AI;
      (Bin, AI)->
        {Ps, Acc} =
        case binary:split(Bin, [<<$\n, $\n>>], []) of
          [BinPs]-> {BinPs, #multipart{type = form}};
          [BinPs, PartBody] -> {BinPs, #multipart{type = form, body = PartBody}}
        end,
%%        io:format("~p~n", [{Ps, Acc}]),
        case parse_multipart_header(trim_line(Ps), Acc) of
          {ok, MP=#multipart{headers = Headers}}->
            %% Content-Disposition: form-data; name=file; filename=chrome.png\nContent-Type: image/png
            Name =
            case maps:find(?Content_Disposition, Headers) of
              {ok, ContentDisposition}->
                Args = form_data_args(ContentDisposition),
                proplists:get_value(?name, Args, undefined);
              _Err->undefined
            end,
            case Name of
              undefined->AI;
              _-> [{Name, MP}|AI]
            end;
          _->AI
        end
    end, [], binary:split(Body, Boundary, [global])).  %% <<Boundary/binary, $\n>>
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Name/Len?[Key=Val]\nBody\nName/Len?[Key=Val]\nBody
compact_parse(Body)->
  compact_parse(Body, []).

compact_parse(<<>>, Acc)->
  {ok, Acc};
compact_parse(Body, AccIn)->
  {Head, LeftBody} =
  case binary:split(Body, [<<$\n>>], []) of
    [A]->{A, <<>>};
    [A,B]->{A, B}
  end,
  {LNs, Args} = compact_Head(Head),
  case LNs of
    [Name, LenB|_]->
      case string:to_integer(LenB) of
        {error, _X}->{error, {length, LNs}};
        {Len, _} ->
          LenLB = byte_size(LeftBody),
          Headers = parse_query_map(Args, #{path => LNs}),
          case Len >= LenLB of
            true-> {ok, [{Name, #multipart{type=compact, headers = Headers, body = LeftBody}}|AccIn]};
            _->
              compact_parse(trim_all(binary_part(LeftBody, {Len, LenLB-Len}))
                , [{Name, #multipart{type=compact, headers = Headers, body = binary_part(LeftBody, {0, Len})}}|AccIn])
          end
      end;
    _->
      {error, {length, LNs}}
  end.


compact_Head(Head)->
  {LN, Args} =
  case binary:split(Head, [<<$?>>], []) of
    [A]->{A, <<>>};
    [A,B]->{A, B}
  end,
  {binary:split(LN, [<<$/>>], [global]), Args}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%