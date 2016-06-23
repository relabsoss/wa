-module(persist_redis).
-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([
          q/2, 
          qp/2, 
          start_link/1, 
          init/1, 
          handle_call/3, 
          handle_cast/2, 
          handle_info/2, 
          terminate/2, 
          code_change/3
        ]).

-include("wa.hrl").

%
% external
%
 
start_link(Params) -> 
  gen_server:start_link(?MODULE, Params, []).

q(Pool, Query) -> 
  poolboy:transaction(Pool, fun(Worker) ->
      gen_server:call(Worker, {q, Query})
    end).

qp(Pool, Pipeline) ->
  poolboy:transaction(Pool, fun(Worker) ->
      gen_server:call(Worker, {qp, Pipeline})
    end).   

init([Params]) ->
  process_flag(trap_exit, true),
  try_redis(ok, Params).

handle_call({q, _Query}, _From, undefined) -> 
  {reply, {error, noconnection}, undefined};
handle_call({q, Query}, _From, C) -> 
  {reply, eredis:q(C, Query), C};
handle_call({qp, _Pipeline}, _From, undefined) -> 
  {reply, {error, noconnection}, undefined};
handle_call({qp, Pipeline}, _From, C) -> 
  {reply, eredis:qp(C, Pipeline), C};
handle_call(_Msg, _From, State) -> 
  {reply, ok, State}.

handle_cast(_Msg, State) -> 
  {noreply, State}.

handle_info({reinit, Params}, undefined) -> 
  try_redis(noreply, Params);
handle_info(_Info, State) -> 
  {noreply, State}.

terminate(_Reason, undefined) -> 
    ok;
terminate(_Reason, Pid) -> 
  ?INFO("Redis connection down ~p", [Pid]),
  eredis:stop(Pid),
  ok.

code_change(_OldVsn, State, _Extra) -> 
  {ok, State}.

%
% local
%

try_redis(RAtom, Params) ->
  case init_redis(Params) of
    {ok, RPid} ->
      ?INFO("Redis connection up ~p", [RPid]),
      {RAtom, RPid};
    {error, undefined} ->
      ?INFO("Redis misconfigured, not running"),
      {RAtom, undefined};
    Reason ->
      ?ERROR("Error in connection to Redis server - ~p", [Reason]),
      ?AFTER(?RECONNECT_TIMEOUT, {reinit, Params}),
      {RAtom, undefined}
  end.

init_redis(undefined) -> 
  {error, undefined};
init_redis(#{ host := Host, port := Port, db := Database, password := Password }) -> 
  eredis:start_link(Host, Port, Database, Password);
init_redis(#{ host := Host, port := Port, db := Database }) -> 
  eredis:start_link(Host, Port, Database);
init_redis(#{ host := Host, port := Port }) -> 
  eredis:start_link(Host, Port);
init_redis(_) -> 
  eredis:start_link().
