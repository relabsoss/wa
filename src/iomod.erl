-module(iomod).
-export([
          in/1, 
          out/1, 
          cascade/3,
          out_json/2, 
          out_html/4,
          strip/1,
          pwd_to_db_pwd/1,
          plain_pwd_to_db_pwd/1,
          random/0,
          split/1,
          concat/2,
          floor/1,
          ceiling/1,
          take_one/1
        ]).

-include("wa.hrl").

in(Msg) ->
  in(Msg, #{}).
in(Msg, Default) ->
  try jsx:decode(Msg, [return_maps]) of
    {error, Error} -> 
      ?ERROR("Error ~p in decoding ~p", [Error, Msg]),
      Default;
    {error, Error, Str} -> 
      ?ERROR("Error ~p in decoding ~p", [Error, Str]),
       Default;
    Data -> Data
  catch Exc:Exp -> 
    ?ERROR("Exception ~p:~p in decoding of ~p", [Exc, Exp, Msg]),
    Default 
  end.

out(Msg) ->
  try
    case jsx:encode(Msg) of
      Str when is_binary(Str) -> Str;
      Err -> 
        ?ERROR("Error encoding to JSON ~p in ~p", [Err, Msg]), 
        jsx:encode([])
    end
  catch 
    Exc:Exp -> 
      ?ERROR("Exception ~p:~p in encoding of ~p", [Exc, Exp, Msg]),
      <<"{}">>
  end.
    
cascade(Map, Fields, Op) ->
  maps:map(fun(K, V) -> 
      case lists:any(fun(I) -> I =:= K end, Fields) of
        true -> ?MODULE:Op(V);
        false -> V
      end
    end, Map).

out_json(Msg, Req) ->
  {Type, R} = out(Msg),
  {ok, Req1} = cowboy_req:reply(200, [
        {<<"content-type">>, <<"application/json">>}
      ], R, Req),
  Req1.

out_html(Status, Tmpl, Context, Req) ->
  Headers = [
      {<<"content-type">>, <<"text/html; charset=UTF-8">>}
    ], 
  case Tmpl:render(Context) of
    {ok, Html} ->
      {ok, Req1} = cowboy_req:reply(Status, Headers, Html, Req),
      Req1;
    Any ->
      ?ERROR("Can't render template ~p for context ~p - ~p", [Tmpl, Context, Any]),
      Req
  end.

strip(S) -> 
  re:replace(S, "[<>&/]+", "", [global,{return, binary}]).

pwd_to_db_pwd(MD5Bin) ->
  smd5(<<MD5Bin/binary, (?CONFIG(salt, <<"deadbeef">>))/binary>>).

plain_pwd_to_db_pwd(Bin) when is_binary(Bin) ->
  plain_pwd_to_db_pwd(binary_to_list(Bin));
plain_pwd_to_db_pwd(L) ->
  pwd_to_db_pwd(list_to_binary(smd5(L))).

smd5(S) ->
  lists:flatten([io_lib:format("~2.16.0b", [C]) || <<C>> <= erlang:md5(S)]).

random() ->
  base64:encode(crypto:strong_rand_bytes(?CONFIG(sid_size, 64))).

split(Title) ->
  case re:split(Title, " ", [{return, binary}]) of 
    [N] -> 
      {N, <<"">>};
    [N1, N2] -> 
      {N1, N2};
    L -> 
      [N2 | T] = lists:reverse(L), 
      T1 = lists:reverse(T), 
      {concat(T1, <<" ">>), N2}
  end.

concat([], _) -> 
  <<>>;
concat([E], _) -> 
  E;
concat([A1, A2 | T], Del) -> 
  concat([<<A1/binary, Del/binary, A2/binary>> | T]).

floor(X) when X < 0 ->
  T = trunc(X),
  case X - T == 0 of
    true -> T;
    false -> T - 1
  end;
floor(X) -> 
  trunc(X).

ceiling(X) when X < 0 -> 
  trunc(X);
ceiling(X) ->
  T = trunc(X),
  case X - T == 0 of
    true -> T;
    false -> T + 1
  end.

take_one([H | _]) -> H.
