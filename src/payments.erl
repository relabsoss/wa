%% -*- coding: utf-8 -*-
-module(payments).
-behaviour(gen_server).
-export([
          add_payment/5, 
          complete_payment/2,
          start_link/0, 
          init/1, 
          handle_call/3, 
          handle_cast/2, 
          handle_info/2, 
          terminate/2, 
          code_change/3
        ]).

-include("wa.hrl").

add_payment(Mail, PlanId, Plan, Token, Reply) ->
  gen_server:cast(?MODULE, {add, Mail, PlanId, Plan, Token, Reply}).

complete_payment(Mail, Token) ->
  gen_server:cast(?MODULE, {complete, Mail, Token}).

%
% external
%
 
start_link() -> 
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
  {ok, ?CONFIG(payments, #{})}.

%
% gen_server
%

handle_call(_Msg, _From, State) -> 
  {reply, ok, State}.

handle_cast({add, Mail, Token, Reply}, State) -> 
  persist:add_payment(pgdb, Mail, Token, Reply),
  ?AFTER(?S2MS(?PAYMENT_CHECK), {check, Mail, Token, 0}),
  {noreply, State};
handle_cast({complete, Mail, Token}, State) -> 
  ?AFTER(0, {check, Mail, Token, ?PAYMENT_CHECK_MAX - 1}),        
  {noreply, State};
handle_cast(_Msg, State) -> 
  {noreply, State}.

handle_info({check, Mail, Token, N}, State) when N < ?PAYMENT_CHECK_MAX -> 
  try check(Mail, Token, State) of
    repeat -> 
      ?AFTER(?S2MS(?PAYMENT_CHECK), {check, Mail, Token, N + 1});
    Any ->
      ?INFO("Check for ~p payment with token ~p return ~p", [Mail, Token, Any]) 
  catch
    _:_ ->
      ?AFTER(?S2MS(?PAYMENT_CHECK), {check, Mail, Token, N + 1})
  end,
  {noreply, State};
handle_info(_Info, State) -> 
  {noreply, State}.

terminate(_Reason, _State) -> 
  ok.

code_change(_OldVsn, State, _Extra) -> 
  {ok, State}.

%
% check payment
% 

check(Mail, Token, _Opts) ->
  case persist:get_payment(pgdb, Mail, Token) of
    #{ <<"state">> := <<"start">> } ->
      case paypal_api:obtain_details(paypal, #{ token => Token }) of
        {ok, DetailsReply, T} ->
          timer:sleep(?S2MS(?WAIT_BEFORE_PAY)),
          paypal_api:complete(paypal, T#{ req => [] }, fun(Reply, _) ->
              Success = fun() ->
                %
                % Success path
                %
                <<"success">>
              end,
              State = case maps:get(<<"ACK">>, Reply) of
                <<"Success">> -> Success();
                <<"SuccessWithWarning">> -> Success();
                _ -> <<"fail">>
              end,
              persist:set_payment_state(pgdb, Mail, Token, State, DetailsReply, Reply)
          end),
          repeat;
        Any ->
          ?WARNING("Try PayPal's detail, got ~p", [Any]),
          repeat
      end;
    _ ->
      no_job
  end.
