-module(pi_handler).
-behaviour(cowboy_http_handler).
-export([
          init/3, 
          handle/2, 
          terminate/3
        ]).

-include("wa.hrl").

init({tcp, http}, Req, Param) ->
  {ok, Req1, Pid} = session:check(Req),
  {ok, Req1, {Param, Pid}}.

handle(Req, {Param, SPid} = State) ->
  {PathInfo, Req1} = cowboy_req:path_info(Req),
  {Context, Req2} = each(SPid, Req1),
  Req3 = process(Param, PathInfo, SPid, Context, Req2),
  {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
  ok.

%
% processing
%

each(SPid, Req) ->
  {Path, Req1} = cowboy_req:path(Req),
  session:current(SPid, Path),
  UI = session:user_info(SPid),
  {#{ user => UI }, Req1}.

process(index, _PathInfo, _SPid, Context, Req) ->
  iomod:out_html(200, index_dtl, Context, Req);

process(Param, PathInfo, _SPid, Context, Req) ->
  ?INFO("Can't process ~p:~p", [Param, PathInfo]),
  iomod:out_html(404, index_dtl, Context, Req).
