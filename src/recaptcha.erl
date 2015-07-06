-module(recaptcha).

-export([check/2]).

-include("wa.hrl").
-include_lib("deps/alog/include/alog.hrl").

-define(URL, "https://www.google.com/recaptcha/api/siteverify").


check(IP, Values) ->
    Response = proplists:get_value(<<"g-recaptcha-response">>, Values, <<"">>),
    case (byte_size(Response) =:= 0) of
        true ->
            ?DEBUG("One or more params of Recaptcha are zero length (~p)", [Response]),
            false;
        false ->
            api_call(?RECAPTCHA_KEY, IP, Response)
    end.


api_call(Key, IP, Response) ->
    case restc:request(post, percent, ?URL, [200], [],
            [
                {<<"secret">>, Key},
                {<<"remoteip">>, IP}, 
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
