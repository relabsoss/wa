-module(oauth2c).
-export([
          access_token/5,
          request/7
        ]).

request(Method, Type, Url, Expect, Headers, Body, Client) ->
  case do_request(Method, Type, Url, Expect, Headers, Body, Client) of
    {{_, 401, _, _}, Client2} ->
      {ok, _RetrHeaders, Client3} = do_retrieve_access_token(Client2),
      do_request(Method, Type, Url, Expect, Headers, Body, Client3);
    Result -> 
      Result
  end.

do_retrieve_access_token(#{ grant_type := <<"password">> } = Client) ->
    Payload0 = [
                {<<"grant_type">>, Client#client.grant_type}
                ,{<<"username">>, Client#client.id}
                ,{<<"password">>, Client#client.secret}
               ],
    Payload = case Client#client.scope of
                 undefined -> Payload0;
                 Scope -> [{<<"scope">>, Scope}|Payload0]
              end,
    case restc:request(post, percent, binary_to_list(Client#client.auth_url), [200], [], Payload) of
        {ok, _, Headers, Body} ->
            AccessToken = proplists:get_value(<<"access_token">>, Body),
            RefreshToken = proplists:get_value(<<"refresh_token">>, Body),
            Result = case RefreshToken of
                undefined ->
                    #client{
                            grant_type    = Client#client.grant_type
                            ,auth_url     = Client#client.auth_url
                            ,access_token = AccessToken
                            ,id           = Client#client.id
                            ,secret       = Client#client.secret
                            ,scope        = Client#client.scope
                            };
                _ ->
                    #client{
                            grant_type     = Client#client.grant_type
                            ,auth_url      = Client#client.auth_url
                            ,access_token  = AccessToken
                            ,refresh_token = RefreshToken
                            ,scope         = Client#client.scope
                            }
            end,
            {ok, Headers, Result};
        {error, _, _, Reason} ->
            {error, Reason};
        {error, Reason} ->
            {error, Reason}
    end;
do_retrieve_access_token(#client{grant_type = <<"client_credentials">>,
                                 id = Id, secret = Secret} = Client) ->
    Payload0 = [{<<"grant_type">>, Client#client.grant_type}],
    Payload = case Client#client.scope of
                  undefined ->
                      Payload0;
                  Scope ->
                      [{<<"scope">>, Scope}|Payload0]
              end,
    Auth = base64:encode(<<Id/binary, ":", Secret/binary>>),
    Header = [{"Authorization", binary_to_list(<<"Basic ", Auth/binary>>)}],
    case restc:request(post, percent, binary_to_list(Client#client.auth_url),
                       [200], Header, Payload) of
        {ok, _, Headers, Body} ->
            AccessToken = proplists:get_value(<<"access_token">>, Body),
            TokenType = proplists:get_value(<<"token_type">>, Body, <<>>),
            Result = #client{
                             grant_type    = Client#client.grant_type
                             ,auth_url     = Client#client.auth_url
                             ,access_token = AccessToken
                             ,token_type   = get_token_type(TokenType)
                             ,id           = Client#client.id
                             ,secret       = Client#client.secret
                             ,scope        = Client#client.scope
                            },
            {ok, Headers, Result};
        {error, _, _, Reason} ->
            {error, Reason};
        {error, Reason} ->
            {error, Reason}
    end.

-spec get_token_type(binary()) -> token_type().
get_token_type(Type) ->
    get_str_token_type(string:to_lower(binary_to_list(Type))).

-spec get_str_token_type(string()) -> token_type().
get_str_token_type("bearer") -> bearer;
get_str_token_type(_Else) -> unsupported.

do_request(Method, Type, Url, Expect, Headers, Body, Client) ->
    Headers2 = add_auth_header(Headers, Client),
    {restc:request(Method, Type, binary_to_list(Url), Expect, Headers2, Body), Client}.

add_auth_header(Headers, #client{access_token = AccessToken, token_type = TokenType}) ->
    Prefix = autorization_prefix(TokenType),
    AH = {"Authorization", binary_to_list(<<Prefix/binary, " ", AccessToken/binary>>)},
    [AH | proplists:delete("Authorization", Headers)].

-spec autorization_prefix(token_type()) -> binary().
autorization_prefix(bearer) -> <<"Bearer">>;
autorization_prefix(unsupported) -> <<"token">>.
