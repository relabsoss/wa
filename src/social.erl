-module(social).
-export([
          fetch/2
        ]).

-include("wa.hrl").

fetch(<<"facebook">>, #{ access_token := Token, token_type := Prefix } = _Props) ->
  case restc:request(get, json, "https://graph.facebook.com/me", [200], 
      #{ <<"Authorization">> => <<Prefix/binary, " ", Token/binary>> }, 
      #{ <<"access_token">> => Token, 
         <<"fields">> => <<"id,email,name">> }) of
    {ok, _, _, Resp} ->
      case maps:get(<<"error">>, Resp, undefined) of
        undefined ->
          Id = maps:get(<<"id">>, Resp, <<>>),
          Name = maps:get(<<"name">>, Resp, <<>>),
          {FName, LName} = iomod:split(Name),
          {ok, #{
                  id => <<Id/binary, "@facebook.com">>,
                  mail => maps:get(<<"email">>, Resp, <<>>),
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
      #{ <<"Authorization">> => <<Prefix/binary, " ", Token/binary>> },
      #{}) of
    {ok, _, _, Resp} ->
      case maps:get(error, Resp, undefined) of
        undefined ->
          Id = maps:get(<<"id">>, Resp, <<>>),
          Name = maps:get(<<"name">>, Resp, <<>>),
          {FName, LName} = iomod:split(Name),
          {ok, #{
                  id => <<Id/binary, "@google.com">>,
                  mail => maps:get(<<"email">>, Resp, <<>>),
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
      #{ <<"Authorization">> => <<Prefix/binary, " ", Token/binary>> }, 
      #{ <<"access_token">> => Token,
         <<"fields">> => <<"uid,first_name,last_name">> }) of
    {ok, _, _, Resp} ->
      case maps:get(error, Resp, undefined) of
        undefined ->
          [R] = maps:get(<<"response">>, Resp, [[]]),
          Id = integer_to_binary(maps:get(<<"uid">>, R, 0)),
          FName = maps:get(<<"first_name">>, R, <<>>),
          LName = maps:get(<<"last_name">>, R, <<>>),
          Mail = case maps:get(email, Props, undefined) of
            undefined -> 
              <<"id", Id/binary, "@vk.com">>;
            Val -> 
              Val
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

    