-module(recaptcha).
-export([
          check/3
        ]).

-include("wa.hrl").
-define(URL, "https://www.google.com/recaptcha/api/siteverify").

check(IP, Values, Key) ->
  Response = maps:get(<<"g-recaptcha-response">>, Values, <<"">>),
  case (byte_size(Response) =:= 0) of
    true ->
      ?DEBUG("One or more params of Recaptcha are zero length (~p)", [Response]),
      false;
    false ->
      api_call(Key, IP, Response)
  end.

api_call(Key, IP, Response) ->
  case restc:request(post, percent, ?URL, [200], [],
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
