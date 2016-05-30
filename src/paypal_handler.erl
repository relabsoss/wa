%% -*- coding: utf-8 -*-
-module(paypal_handler).
-behaviour(cowboy_http_handler).
-export([
          init/3, 
          handle/2, 
          terminate/3
        ]).

-include("wa.hrl").

init({tcp, http}, Req, _Opts) ->
  {ok, Req1, SID} = session:check(Req),
  {ok, Req1, SID}.

handle(Req, Pid) ->
  {PathInfo, Req1} = cowboy_req:path_info(Req),
  {ok, Req2} = case maps:from_list(session:user_info(Pid)) of
    #{ auth := true } = UI -> 
      process(PathInfo, UI, Req1);
    Any -> 
      ?INFO("Got user info as ~p", [Any]),
      cowboy_req:reply(403, [], <<>>, Req1)
  end,
  {ok, Req2, Pid}.

terminate(_Reason, _Req, _State) ->
  ok.

%
% process
%

process([<<"token">>], UI, Req) ->
  % change processing list here
  R = case paypal_api:get_token_url(paypal, [#{ amount => 1, currency => "USD" }]) of
    {ok, Reply, T} ->
      Mail = maps:get(mail, UI), 
      URL = maps:get(redirect_url, T),
      Token = maps:get(token, T),
      payments:add_payment(Mail, Token, Reply),
      #{ 
        result => ok, 
        redirect => URL 
      };
    Any ->
      ?ERROR("Got ~p", [Any]),
      #{ 
        result => error, 
        error => <<"paypal_api">> 
      }
  end,
  {ok, iomod:out_json(R, Req)};
process([Result], UI, Req) when (Result =:= <<"success">>) or (Result =:= <<"fail">>) ->
  {Token, Req1} = cowboy_req:qs_val(<<"token">>, Req, <<"">>),
  Mail = maps:get(mail, UI), 
  URL = maps:get(current_page, UI, <<"/">>),
  payments:complete_payment(Mail, Token),
  cowboy_req:reply(302, [{<<"location">>, URL}], <<>>, Req1);
process(Any, _, Req) ->
  ?WARNING("Can't process ~p", [Any]),
  {ok, Req}.
