-module(recaptcha).

-export([check/2]).

-include("wa.hrl").
-include_lib("deps/alog/include/alog.hrl").

-define(URL, "http://www.google.com/recaptcha/api/verify").


check(IP, Values) ->
    Challenge = proplists:get_value(<<"recaptcha_challenge_field">>, Values, <<"">>),
    Response = proplists:get_value(<<"recaptcha_response_field">>, Values, <<"">>),
    case (byte_size(Challenge) =:= 0) or (byte_size(Response) =:= 0) of
        true ->
            ?DEBUG("One or more params of Recaptcha are zero length (~p | ~p)", [Challenge, Response]),
            false;
        false ->
            api_call(?RECAPTCHA_KEY, IP, Challenge, Response)
    end.


api_call(Key, IP, Challenge, Response) ->
    case restc:request(post, percent, ?URL, [200], [],
            [
                {<<"privatekey">>, Key},
                {<<"remoteip">>, IP}, 
                {<<"challenge">>, Challenge}, 
                {<<"response">>, Response}
            ]) of
        {ok, _Status, _Headers, Body} ->
            case string:tokens(Body, "\n") of
                ["true"] -> true;
                ["true", _] -> true;
                ["false", Reason] ->
                    ?ERROR("Recaptcha fail with the reason: ~p", [Reason]),
                    false;
                Any ->
                    ?ERROR("Unknown server reply: ~p", [Any]),
                    false
            end;
        Error ->
            ?ERROR("Server error: ~p", [Error]),
            false
    end.
