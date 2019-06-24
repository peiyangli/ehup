%%%-------------------------------------------------------------------
%%% @author pei
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. 十一月 2018 20:03
%%%-------------------------------------------------------------------
-module(ehup_utils).
-author("pei").

%% API
-export([path/2, merge/2, format/2]).

%%-compile(export_all).

nth(1, [H|_], _) -> H;
nth(N, [_|T], Def) when N > 1 ->
  nth(N - 1, T, Def);
nth(N, Tuple, Def) when is_tuple(Tuple)->
  try
    erlang:element(N, Tuple)
  catch
    _:_  ->Def
  end;
nth(_, _, Def)->Def.

%%%% get deep map value by path

path(Path, Map)->
  path(Path, Map, undefined).

path([], _, Def)->
  Def;
path([Key], Map, Def) when is_map(Map)->
  maps:get(Key, Map, Def);
path([Key|Path], Map, Def) when is_map(Map)->
  path(Path, maps:get(Key, Map), Def);
path([Key], List, Def) when is_integer(Key)->
  nth(Key, List, Def);
path([Key|Path], List, Def) when is_integer(Key)->
  path(Path, nth(Key, List, undefined), Def);
path(_, _, Def)->
  Def.



% deep merge of maps. when the value is both map, we do it deep merge
% replace A's with B's if the key is same
merge(A, B) when is_map(A) andalso is_map(B)->
  maps:fold(
    fun(K, V, Acc) when is_map(V)->
      case maps:find(K, Acc) of
        {ok, Value} when is_map(Value)->
          Acc#{K => merge(Value, V)};
        _->
          Acc#{K => V}
      end;
      (K, V, Acc)->
        Acc#{K => V}
    end, A, B).


format(Fmt, Args)when is_binary(Fmt)->
  list_to_binary(io_lib:format(Fmt, Args));
format(Fmt, Args)->
  lists:flatten(io_lib:format(Fmt, Args)).