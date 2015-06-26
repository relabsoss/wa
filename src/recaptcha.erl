-module(recaptcha).

-export([check/2]).

-include("wa.hrl").
-include_lib("deps/alog/include/alog.hrl").

-define(HOST, "www.google.com").
-define(URL, "/recaptcha/api/verify").


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
    Body = << 
            <<"privatekey=">>/binary, 
            Key/binary, 
            <<"&remoteip=">>/binary, 
            IP/binary, 
            <<"&challenge=">>/binary, 
            Challenge/binary, 
            <<"&response=">>/binary, 
            Response/binary
        >>,
    {ok, Conn} = shotgun:open(?HOST, 80),
    {ok, Response} = shotgun:post(Conn, ?URL, [
            {"Content-Type", "application/x-www-form-urlencoded;"},                    
            {"Content-Length", byte_size(Body)}
            ], Body, #{}),
    case maps:get(status_code, Response) of
        200 ->
            case string:tokens(maps:get(body, Response), "\n") of
                ["true"] -> true;
                ["true", _] -> true;
                ["false", Reason] ->
                    ?ERROR("Recaptcha fail with the reason: ~p", [Reason]),
                    false;
                Any ->
                    ?ERROR("Unknown server reply: ~p", [Any]),
                    false
            end;
        Status ->
            ?ERROR("Server error status: ~p", [Status]),
            false
    end,
    shotgun:close(Conn).
