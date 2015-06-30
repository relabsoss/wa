-module(social_handler).
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

-include("wa.hrl").
-include_lib("deps/alog/include/alog.hrl").


init({tcp, http}, Req, Opts) ->
    {Provider, Req2} = cowboy_req:binding(provider, Req),
    {Action, Req3} = cowboy_req:binding(action, Req2, <<"login">>),
    case proplists:get_value(Provider, Opts, false) of
        false ->
            {ok, Req4} = cowboy_req:reply(404, Req3),
            {shutdown, Req4, undefined};
        Options ->
            {Req4, Options1} = case maps:get(callback_uri, Options) of
                <<"http://", _/binary>> -> {Req3, Options};
                <<"https://", _/binary>> -> {Req3, Options};
                Relative -> 
                    {Headers, Req5} = cowboy_req:headers(Req3),
                    Options2 = Options#{ callback_uri := <<
                        (proplists:get_value(<<"x-scheme">>, Headers, <<"http">>))/binary,
                        "://",
                        (proplists:get_value(<<"host">>, Headers))/binary,
                        Relative/binary>>},
                    {Req5, Options2}
            end,
            {ok, Req4, #{
                    action => Action,
                    provider => Provider,
                    options => Options1
                }}
    end.

handle(Req, #{action := <<"login">>, options := Options}) ->
    URL = maps:get(authorize_uri, Options),
    Params = [
            {<<"client_id">>, maps:get(client_id, Options)},
            {<<"redirect_uri">>, maps:get(callback_uri, Options)},
            {<<"response_type">>, <<"code">>},
            {<<"scope">>, maps:get(scope, Options)}
    ],    
    {ok, Req2} = cowboy_req:reply(302, [{
            <<"location">>, <<URL/binary, $?, (cow_qs:qs(Params))/binary>>}
        ], <<>>, Req),
    {ok, Req2, undefined};

handle(Req, #{action := <<"callback">>, options := Options, provider := Provider} = State) ->
    case cowboy_req:qs_val(<<"error">>, Req) of
        {undefined, Req2} ->
            case check(Options, Req2) of
                {ok, Props, Req3} -> 
                    CBL = maps:get(callbacks, Options),
                    Req4 = lists:foldl(fun({Mod, Fun}, Req5) -> 
                            Mod:Fun(Provider, Props, Req5)
                        end, Req3, CBL),
                    {ok, Req4, State};
                {reply, Req3} -> 
                    {ok, Req3, State};
                {error, Req3} -> 
                    {ok, fallback(Req3), undefined}
            end;
        {Error, Req2} -> 
            ?ERROR("Error in oauth2 - ~p", [Error]),
            {ok, fallback(Req2), undefined}
    end;    
handle(Req, _State) ->
    {ok, Req1} = cowboy_req:reply(404, Req),
    {ok, Req1, undefined}.    

terminate(_Reason, _Req, _State) ->
    ok.

%
% local
%

fallback(Req) -> 
    URL = ?CONFIG(social_fallback_url, <<"/">>),
    {ok, Req1} = cowboy_req:reply(302, [{<<"location">>, URL}], <<>>, Req),
    Req1.

check(Options, Req) ->
    case cowboy_req:qs_val(<<"code">>, Req) of
        {undefined, Req1} ->
            case cowboy_req:qs_val(<<"access_token">>, Req1) of
                {undefined, Req2} -> 
                    {ok, Req3} = cowboy_req:reply(200, [], 
                        <<"<script>",
                            "window.location.replace(window.location.href.replace('#','?'))",
                        "</script>">>, Req2),
                    {reply, Req3};
                {Token, Req2} ->
                    {TokenType, Req3} = cowboy_req:qs_val(<<"token_type">>, Req2, <<"bearer">>),
                    {ok, #{
                            access_token => Token,
                            token_type => TokenType,
                            refresh_token => undefined       
                        }, Req3}
            end;
        {Code, Req1} ->
            case restc:request(post, percent, binary_to_list(maps:get(token_uri, Options)), [200], [], [
                        {<<"code">>, Code},
                        {<<"client_id">>, maps:get(client_id, Options)},
                        {<<"client_secret">>, maps:get(client_secret, Options)},
                        {<<"redirect_uri">>, maps:get(callback_uri, Options)},
                        {<<"grant_type">>, <<"authorization_code">>}
                    ]) of
                {ok, _Status, _Headers, Body} ->
                    case proplists:get_value(error, Body, undefined) of
                        undefined ->
                            {ok, #{
                                access_token => proplists:get_value(<<"access_token">>, Body, <<>>),
                                token_type => proplists:get_value(<<"token_type">>, Body, <<"bearer">>),
                                refresh_token => proplists:get_value(<<"refresh_token">>, Body, undefined)
                            }, Req1};
                        Error ->
                            ?ERROR("Error in OAuth access tocken request ~p", [Error]),
                            {error, Req1}
                    end;
                Any ->
                    ?ERROR("Error in OAuth access tocken request call ~p", [Any]),
                    {error, Req1}
            end
    end.
