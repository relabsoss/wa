-module(auth_handler).
-behaviour(cowboy_http_handler).
-export([
          init/3, 
          handle/2, 
          terminate/3
        ]).

-include("wa.hrl").

init({tcp, http}, Req, _Opts) ->
  session:check(Req).

handle(Req, Pid) ->
  {PathInfo, Req2} = cowboy_req:path_info(Req),
  {Reply, Req3} = session:process(Pid, PathInfo, Req2),
  Req4 = iomod:out_json(Reply, Req3),
  {ok, Req4, Pid}.

terminate(_Reason, _Req, _State) ->
  ok.
