-module(oauth2c).
-export([
          retrieve_access_token/5,
          request/7
        ]).

retrieve_access_token(Type, Url, ID, Secret, Scope) ->
  Client = #{
      grant_type => Type,
      auth_url   => Url,
      id         => ID,
      secret     => Secret,
      scope      => Scope
    },
  do_retrieve_access_token(Client).

request(Method, Type, Url, Expect, Headers, Body, Client) ->
  case do_request(Method, Type, Url, Expect, Headers, Body, Client) of
    {{_, 401, _, _}, Client2} ->
      {ok, _RetrHeaders, Client3} = do_retrieve_access_token(Client2),
      do_request(Method, Type, Url, Expect, Headers, Body, Client3);
    Result -> 
      Result
  end.

do_retrieve_access_token(#{ 
      auth_url := AuthURL, 
      grant_type := <<"password">> 
    } = Client) ->
  Payload = maps:with([grant_type, username, password, scope], Client),
  case restc:request(post, percent, binary_to_list(AuthURL), [200], #{}, Payload) of
    {ok, _, Headers, Body} ->
      AccessToken = maps:get(<<"access_token">>, Body),
      RefreshToken = maps:get(<<"refresh_token">>, Body, undefined),
      Result = case RefreshToken of
        undefined ->
          maps:put(access_token, AccessToken, Client);                
        _ ->
          maps:put(access_token, AccessToken,
            maps:put(refresh_token, RefreshToken, Client))
      end,
      {ok, Headers, Result};
    {error, _, _, Reason} ->
      {error, Reason};
    {error, Reason} ->
      {error, Reason}
  end;
do_retrieve_access_token(#{
      auth_url := AuthURL, 
      grant_type := <<"client_credentials">>,
      id := Id, 
      secret := Secret
    } = Client) ->
  Payload = maps:with([grant_type, scope], Client),
  Auth = base64:encode(<<Id/binary, ":", Secret/binary>>),
  Header = #{ <<"Authorization">> => <<"Basic ", Auth/binary>> },
  case restc:request(post, percent, binary_to_list(AuthURL), [200], Header, Payload) of
    {ok, _, Headers, Body} ->
      AccessToken = maps:get(<<"access_token">>, Body),
      TokenType = maps:get(<<"token_type">>, Body, <<>>),
      Result = maps:put(access_token, AccessToken,
        maps:put(token_type, get_token_type(TokenType), Client)),
      {ok, Headers, Result};
    {error, _, _, Reason} ->
      {error, Reason};
    {error, Reason} ->
      {error, Reason}
  end.

get_token_type(Type) ->
  case string:to_lower(binary_to_list(Type)) of
    "bearer" -> bearer;
    _        -> unsupported
  end.

do_request(Method, Type, Url, Expect, Headers, Body, Client) ->
  Headers2 = add_auth_header(Headers, Client),
  {restc:request(Method, Type, binary_to_list(Url), Expect, Headers2, Body), Client}.

add_auth_header(Headers, #{ 
      access_token := AccessToken, 
      token_type := TokenType 
    }) ->
  Prefix = autorization_prefix(TokenType),
  maps:put(<<"Authorization">>, <<Prefix/binary, " ", AccessToken/binary>>, Headers).

autorization_prefix(bearer) -> <<"Bearer">>;
autorization_prefix(unsupported) -> <<"token">>.
