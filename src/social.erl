-module(social).

-export([fetch/2]).

-include("wa.hrl").
-include_lib("deps/alog/include/alog.hrl").


fetch(<<"facebook">>, #{ access_token := Token, token_type := Prefix } = _Props) ->
    case restc:request(get, json, "https://graph.facebook.com/me", [200], 
            [{"Authorization", binary_to_list(<<Prefix/binary, " ", Token/binary>>)}], 
            [{<<"access_token">>, Token},
             {<<"fields">>, <<"id,email,name">>}]) of
        {ok, _, _, Resp} ->
            case proplists:get_value(error, Resp, undefined) of
                undefined ->
                    Id = proplists:get_value(<<"id">>, Resp, <<>>),
                    Mail = proplists:get_value(<<"email">>, Resp, <<>>),
                    Name = proplists:get_value(<<"name">>, Resp, <<>>),
                    {FName, LName} = split(Name),
                    {ok, #{
                        id => <<Id/binary, "@facebook.com">>,
                        mail => Mail,
                        fname => FName,
                        lname => LName,
                        title => Name,
                        token => Token
                    }};
                Error ->
                    ?ERROR("Can't get facebook user info ~p", [Error]),
                    {error, Error}
            end;
        Error ->
            ?ERROR("Can't make facebook user info request ~p", [Error]),
            Error
    end;

fetch(<<"google">>, #{ access_token := Token, token_type := Prefix } = _Props) ->
    case restc:request(get, json, "https://www.googleapis.com/oauth2/v1/userinfo", [200], 
            [{"Authorization", binary_to_list(<<Prefix/binary, " ", Token/binary>>)}]) of
        {ok, _, _, Resp} ->
            case proplists:get_value(error, Resp, undefined) of
                undefined ->
                    Id = proplists:get_value(<<"id">>, Resp, <<>>),
                    Mail = proplists:get_value(<<"email">>, Resp, <<>>),
                    Name = proplists:get_value(<<"name">>, Resp, <<>>),
                    {FName, LName} = split(Name),
                    {ok, #{
                        id => <<Id/binary, "@google.com">>,
                        mail => Mail,
                        fname => FName,
                        lname => LName,
                        title => Name,
                        token => Token
                    }};
                Error ->
                    ?ERROR("Can't get google user info ~p", [Error]),
                    {error, Error}
            end;
        Error ->
            ?ERROR("Can't make google user info request ~p", [Error]),
            Error
    end;

fetch(<<"vk">>, #{ access_token := Token, token_type := Prefix } = Props) ->
    case restc:request(get, json, "https://api.vk.com/method/users.get", [200], 
            [{"Authorization", binary_to_list(<<Prefix/binary, " ", Token/binary>>)}], 
            [{<<"access_token">>, Token},
             {<<"fields">>, <<"uid,first_name,last_name">>}]) of
        {ok, _, _, Resp} ->
            case proplists:get_value(error, Resp, undefined) of
                undefined ->
                    [R] = proplists:get_value(<<"response">>, Resp, [[]]),
                    Id = integer_to_binary(proplists:get_value(<<"uid">>, R, 0)),
                    FName = proplists:get_value(<<"first_name">>, R, <<>>),
                    LName = proplists:get_value(<<"last_name">>, R, <<>>),
                    Mail = case maps:get(email, Props, undefined) of
                        undefined -> <<"id", Id/binary, "@vk.com">>;
                        Val -> Val
                    end,
                    {ok, #{
                        id => <<Id/binary, "@vk.com">>,
                        mail => Mail,
                        fname => FName,
                        lname => LName,
                        title => <<FName/binary, " ", LName/binary>>,
                        token => Token
                    }};
                Error ->
                    ?ERROR("Can't get facebook user info ~p", [Error]),
                    {error, Error}
            end;
        Error ->
            ?ERROR("Can't make facebook user info request ~p", [Error]),
            Error
    end.

%
% Local
% 

split(Title) ->
    case re:split(Title, " ", [{return, binary}]) of 
        [N] -> {N, <<"">>};
        [N1, N2] -> {N1, N2};
        L -> [N2|T] = lists:reverse(L), T1 = lists:reverse(T), {concat(T1), N2}
    end.

concat([]) -> <<>>;
concat([E]) -> E;
concat([A1, A2 | T]) -> concat([<<A1/binary, " ", A2/binary>> | T]).


    