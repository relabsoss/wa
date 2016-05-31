-module(restc).
-export([
          request/6
        ]).

-include("wa.hrl").

request(Method, Type, URL, Expect, InHeaders, Body) ->
  Headers = maps:merge(InHeaders, #{
      <<"Accept">> => get_access_type(Type) ++ ", */*;q=0.9",
      <<"Content-Type">> => get_content_type(Type)
    }),  
  {RURL, RHeaders, RBody} = case lists:any(fun(I) -> I =:= Method end, [post, put]) of  
    true -> 
      EncBody = encode_body(Type, Body),
      {
        URL, 
        maps:put("content-length", byte_size(EncBody), Headers),
        EncBody
      };
    false ->
      {ok, {_, _, _, _, _, QS}} = http_uri:parse(URL),
      case QS of
        [] ->
          {
            lists:concat([URL, "?", binary_to_list(cow_qs:qs(maps:to_list(Body)))]), 
            Headers,
            <<>>
          };
        _ ->
          {
            lists:concat([URL, "&", binary_to_list(cow_qs:qs(maps:to_list(Body)))]), 
            Headers,
            <<>>
          }
      end
  end,
  ?DEBUG("OUT >>> ~p === ~p ===  ~p", [RURL, RHeaders, RBody]),
  case hackney:request(Method, RURL, maps:to_list(RHeaders), RBody, []) of
    {ok, Status, RespHeaders, Client} ->
      case hackney:body(Client) of
        {ok, RespBody} ->
          ?DEBUG("IN <<< ~p === ~p === ~p", [Status, RespHeaders, RespBody]), 
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
  end.

get_access_type(html)   -> "text/html";
get_access_type(qs)     -> "application/x-www-form-urlencoded";
get_access_type(_)      -> "application/json".

get_content_type(html)  -> "text/html";
get_content_type(qs)    -> "application/x-www-form-urlencoded";
get_content_type(_)     -> "application/json".

encode_body(qs, Body) -> 
  cow_qs:qs(maps:to_list(Body));
encode_body(html, Body) -> 
  Body;
encode_body(_, Body) -> 
  iomod:out(Body).

parse(HeadersL, Body) ->
  Headers = maps:from_list(iomod:keys_to_lower(HeadersL)),
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
      ?INFO(" [ ~p , ~p ] ", [Type, CType]),
      parse_body(CType, Unzip)
  end.

parse_body("application/json", Body) -> 
  iomod:in(Body); 
parse_body("application/x-www-form-urlencoded", Body) -> 
  maps:from_list(cow_qs:parse_qs(Body));
parse_body("text/plain", Body) -> 
  maps:from_list(cow_qs:parse_qs(Body)); % damn you, Facebook
parse_body(_, Body) -> 
  Body.

