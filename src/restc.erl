-module(restc).
-export([request/6]).

request(Method, Type, Url, InHeaders, Expect, Body) ->
  Headers = maps:merge(InHeaders, #{
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
          maps:to_list(maps:put("content-length", byte_size(EncBody), Headers)), 
          EncBody);
    false when is_list(Body) and (Body =/= []) and (length(QS) =:= 0) ->
      gun:Method(
          ConnPid, 
          lists:concat([Path, "?", binary_to_list(cow_qs:qs(Body))]), 
          maps:to_list(Headers));
    false when is_list(Body) and (Body =/= []) and (length(QS) =/= 0) ->
      gun:Method(
          ConnPid, 
          lists:concat([Path, QS, "&", binary_to_list(cow_qs:qs(Body))]), 
          maps:to_list(Headers));
    false -> 
      gun:Method(
          ConnPid, 
          Path ++ QS, 
          maps:to_list(Headers))
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
          end;
        Error ->
          {error, Error}
      end;
    Any -> 
      Any
  end,
  gun:shutdown(ConnPid),
    
  Resp.

get_access_type(html)   -> "text/html";
get_access_type(_)      -> "application/json".

get_content_type(qs)    -> "application/x-www-form-urlencoded";
get_content_type(html)  -> "text/html";
get_content_type(_)     -> "application/json".

encode_body(ws, Body) -> 
  cow_qs:qs(Body);
encode_body(html, Body) -> 
  Body;
encode_body(_, Body) -> 
  iomod:out(Body).

parse(HeadersL, Body) ->
  Headers = maps:from_list(HeadersL),
  Unzip = case maps:get(<<"content-encoding">>, Headers, <<"plain">>) of
    <<"gzip">> -> 
      zlib:gunzip(Body);
    _ -> 
      Body
  end,
  case maps:get(<<"content-type">>, Headers, undefined) of
    undefined -> 
      parse_body("application/json", Unzip);
    Type -> 
      CType = iomod:take_one(string:tokens(binary_to_list(Type), ";")),
      parse_body(CType, Unzip)
  end.

parse_body("application/json", Body) -> 
  iomod:in(Body); 
parse_body("application/x-www-form-urlencoded", Body) -> 
  cow_qs:parse_qs(Body);
parse_body("text/plain", Body) -> 
  cow_qs:parse_qs(Body); % damn you, Facebook
parse_body(_, Body) -> 
  Body.
