-module(restc).
-export([request/5]).

request(Method, Type, Url, InHeaders, Expect, Body) ->
  maps:merge(InHeaders, #{
      <<"Accept">> => get_access_type(Type) ++ ", */*;q=0.9",
      <<"Content-Type">> => get_content_type(Type)
    }),  
  {ok, {_, _, Host, Port, Path, QS}} = http_uri:parse(Url),
  
  {ok, ConnPid} = gun:open(Host, Port, #{ protocols => [http] }),
  {ok, _Protocol} = gun:await_up(ConnPid),
  StreamRef = case lists:any(fun(I) -> I =:= Method end, [post, put]) of  
    true -> 
      EncBody = encode_body(Type, Body),
      gun:Method(
          ConnPid, 
          Path ++ QS, 
          maps:put("content-length", byte_size(EncBody), Headers), 
          EncBody);
    false when is_list(Body) and (Body =/= []) and (length(QS) =:= 0) ->
      gun:Method(
          ConnPid, 
          lists:concat([Path, "?", binary_to_list(cow_qs:qs(Body))]), 
          Headers);
    false when is_list(Body) and (Body =/= []) and (length(QS) =/= 0) ->
      gun:Method(
          ConnPid, 
          lists:concat([Path, QS, "&", binary_to_list(cow_qs:qs(Body))]), 
          Headers);
    false -> 
      gun:Method(
          ConnPid, 
          Path ++ QS, 
          Headers)
  end,
  Resp = case gun:await(ConnPid, StreamRef) of
    {response, fin, Status, RespHeaders} ->
      {ok, Status, RespHeaders, []};
    {response, nofin, Status, RespHeaders} ->
      case gun:await_body(ConnPid, StreamRef) of
        {ok, RespBody} -> 
          case lists:any(fun(I) -> I =:= Status end, Expect) of
            true ->
              {ok, Status, RespHeaders, parse(RespHeaders, RespBody)};
            false when Expect =:= [] ->
              {ok, Status, RespHeaders, parse(RespHeaders, RespBody)};
            false ->
              {error, Status, RespHeaders, parse(RespHeaders, RespBody)}               
        Error ->
          {error, Error}
      end;
    Any -> Any
  end,
  gun:shutdown(ConnPid),
    
  Resp.

get_access_type(html)   -> "text/html";
get_access_type(_)      -> "application/json".

get_content_type(qs)    -> "application/x-www-form-urlencoded";
get_content_type(html)  -> "text/html";
get_content_type(_)     -> "application/json".

encode_body(ws, Body)   -> cow_qs:qs(Body);
encode_body(html, Body) -> Body;
encode_body(_, Body)    -> iomod:out(Body).

parse(Headers, Body) ->
  ?INFO(""),
  Unzip = case proplists:get_value(<<"content-encoding">>, Headers, <<"plain">>) of
    <<"gzip">> -> zlib:gunzip(Body);
    _ -> Body
  end,
  Type = case lists:keyfind(<<"content-type">>, 1, Headers) of
    false    -> "application/json";
    {_, Val} -> Val
  end,
  CType = case string:tokens(binary_to_list(Type), ";") of
    [CVal]    -> CVal;
    [CVal, _] -> CVal
  end,
  parse_body(CType, Unzip).

parse_body("application/json", Body) -> iomod:in(Body); 
parse_body("application/x-www-form-urlencoded", Body) -> cow_qs:parse_qs(Body);
parse_body("text/plain", Body)       -> cow_qs:parse_qs(Body); % damn you, Facebook
parse_body(_, Body)                  -> Body.
