%%%-------------------------------------------------------------------
%%% @author pei
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. 五月 2019 10:21
%%%-------------------------------------------------------------------
-author("pei").

-ifndef(ehup_h).
-define(ehup_h, true).
%% API

%% see golang hup
-record(response, {status=200, headers=#{}, body= <<>>}).
-record(net, {sock, ip, port}).
-record(request, {method, path= <<>>, query= <<>>, headers= #{}, body= <<>>, multipart}).
-record(context, {request, net= #net{}, handlers=[], status=#{}, response=#response{}}).


-record(multipart, {type, headers= #{}, body= <<>>}).

-define(SERVER_VER, <<"hup/0.1 ">>).

-define(Body_Length, <<"Body-Length">>).
-define(Content_Type, <<"Content-Type">>).
-define(Content_Disposition, <<"Content-Disposition">>).

-endif.