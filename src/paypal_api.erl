%% -*- coding: utf-8 -*-
-module(paypal_api).
-behaviour(gen_server).
-behaviour(poolboy_worker).
-export([
          get_token_url/2, 
          obtain_details/2, 
          complete/3,
          start_link/1, 
          init/1, 
          handle_call/3, 
          handle_cast/2, 
          handle_info/2, 
          terminate/2, 
          code_change/3
        ]).

-include("wa.hrl").

get_token_url(Pool, L) ->
  poolboy:transaction(Pool, fun(Worker) ->
      gen_server:call(Worker, {token_for, L})
    end).
    
obtain_details(Pool, T) ->
  poolboy:transaction(Pool, fun(Worker) ->
      gen_server:call(Worker, {details_for, T})
    end).

complete(Pool, T, Callback) ->
  poolboy:transaction(Pool, fun(Worker) ->
      gen_server:cast(Worker, {complete, T, Callback})
    end).

%
% gen_server
%

start_link(Params) -> 
  gen_server:start_link(?MODULE, Params, []).

init([Params]) ->
  {ok, Params}.

handle_call({token_for, L}, _From, Params) ->
  P = order_to_params(#{
      <<"METHOD">> => <<"SetExpressCheckout">>,
      <<"RETURNURL">> => maps:get(success_url, Params),
      <<"CANCELURL">> => maps:get(fail_url, Params) 
    }, L),
  {ok, Reply, T} = call(P, token, #{ req => L }, Params),
  Token = maps:get(token, T, <<>>),
  RedirectPrefix = maps:get(redirect_prefix, Params),
  {reply, {ok, Reply, T#{ redirect_url => <<RedirectPrefix/binary, Token/binary>> }}, Params};
handle_call({details_for, T}, _From, Params) ->
  {reply, call(#{
      <<"METHOD">> => <<"GetExpressCheckoutDetails">>,
      <<"TOKEN">> => maps:get(token, T, <<>>)
    }, details, T, Params), Params};
handle_call(_Msg, _From, State) -> 
  {reply, ok, State}.

handle_cast({complete, T, Callback}, Params) ->
  case call(order_to_params(#{
        <<"METHOD">> => <<"DoExpressCheckoutPayment">>,
        <<"TOKEN">> => maps:get(token, T, <<>>),
        <<"PAYERID">> => maps:get(payer_id, T, <<>>)
      }, maps:get(req, T, [])), any, T, Params) of
    {ok, Reply, T} -> 
      Callback(Reply, T);
    Any -> 
      ?ERROR("Got Paypal  with ~p", [Any]),
      ok
  end,
  {noreply, Params};
handle_cast(_Msg, State) -> 
  {noreply, State}.

handle_info(_Info, State) -> 
  {noreply, State}.

terminate(_Reason, _State) -> 
  ok.

code_change(_OldVsn, State, _Extra) -> 
  {ok, State}.

%
% local
%

order_to_params(I, L) ->
  {P, _} = lists:foldl(fun(#{ amount := Amount, currency := Currency }, {M, N}) -> 
      NB = integer_to_binary(N), 
      {
        M#{
          <<"PAYMENTREQUEST_", NB/binary, "_PAYMENTACTION">> => <<"SALE">>,
          <<"PAYMENTREQUEST_", NB/binary, "_AMT">> => Amount,
          <<"PAYMENTREQUEST_", NB/binary, "_CURRENCYCODE">> => Currency                
        }, 
        N + 1        
      } 
    end, {I, 0}, L),
  P.

encode(V, Params) ->
  VA = V#{ 
      <<"USER">> => maps:get(user, Params),
      <<"PWD">> => maps:get(password, Params),
      <<"SIGNATURE">> => maps:get(signature, Params),
      <<"VERSION">> => <<"93">> 
    },
  cow_qs:qs(maps:to_list(VA)). 

decode(Body, Process, T) ->
  case {Process, maps:from_list(cow_qs:parse_qs(Body))} of
    {token, #{ <<"TOKEN">> := Token } = M} ->
      {ok, M, T#{ token => Token }};
    {details, #{ <<"PAYERID">> := PayerId } = M} ->
      {ok, M, T#{ payer_id => PayerId }};
    {any, M} ->
      {ok, M, T};
    {_, Any} ->
      ?INFO("Reply - ~p", [Any]),
      error
  end.

call(Values, Process, T, Params) ->
  {ok, ConnPid} = gun:open(maps:get(server, Params), maps:get(port, Params)),
  {ok, _Protocol} = gun:await_up(ConnPid),
  StreamRef = gun:post(ConnPid, maps:get(url, Params), [], encode(Values, Params)),
  Result = case gun:await(ConnPid, StreamRef) of
    {response, fin, Status, RespHeaders} ->
      ?WARNING("No data for request ~p : ~p", [Status, RespHeaders]),
      error;
    {response, nofin, Status, RespHeaders} ->
      case gun:await_body(ConnPid, StreamRef) of
        {ok, Body} -> 
          decode(Body, Process, T);               
        Error ->
          ?ERROR("Error in getting request body ~p for status ~p and headers ~p", [Error, Status, RespHeaders]),
          error
      end;
    {error, Error} ->
      ?ERROR("Got ~p on post", [Error]),
      error
  end,
  gun:shutdown(ConnPid),
  Result.
