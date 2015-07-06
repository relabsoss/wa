-module(recaptcha).

-export([check/3]).

-include("wa.hrl").
-include_lib("deps/alog/include/alog.hrl").

-define(URL, "https://www.google.com/recaptcha/api/siteverify").


check(IP, Values, Key) ->
    Response = proplists:get_value(<<"g-recaptcha-response">>, Values, <<"">>),
    case (byte_size(Response) =:= 0) of
        true ->
            ?DEBUG("One or more params of Recaptcha are zero length (~p)", [Response]),
            false;
        false ->
            api_call(Key, IP, Response)
    end.


api_call(Key, IP, Response) ->
    case restc:request(post, percent, ?URL, [200], [],
            [
                {<<"secret">>, Key},
                {<<"remoteip">>, IP}, 
                {<<"response">>, Response}
            ]) of
        {ok, _Status, _Headers, Body} ->
            case Body of
                [{<<"success">>, true} | _] -> true;
                [{<<"success">>, false}] -> false;
                [{<<"success">>, false},
                 {<<"error-codes">>, Reason}] ->
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
