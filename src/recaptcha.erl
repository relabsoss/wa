-module(recaptcha).
-export([
          check/2,
          check/3
        ]).

-include("wa.hrl").
-define(URL, "https://www.google.com/recaptcha/api/siteverify").

check(IP, Values) ->
  check(IP, Values, maps:get(private, ?CONFIG(recaptcha_key, #{}), <<"">>)).

check(IP, Values, Key) ->
  ?INFO("Values ~p", [Values]),
  Response = maps:get(<<"g-recaptcha-response">>, Values, <<"">>),
  case (byte_size(Response) =:= 0) of
    true ->
      ?INFO("One or more params of Recaptcha are zero length (~p)", [Response]),
      false;
    false ->
      api_call(Key, IP, Response)
  end.

api_call(Key, IP, Response) ->
  case restc:request(post, qs, ?URL, [200], [],
      #{
        <<"secret">> => Key,
        <<"remoteip">> => IP, 
        <<"response">> => Response
      }) of
    {ok, _Status, _Headers, Body} ->
      ?INFO("ReCAPTCHA results - ~p", [Body]),
      maps:get(<<"success">>, Body, false);
    Error ->
      ?ERROR("ReCAPTCHA server error: ~p", [Error]),
      false
    end.
