-module(iomod).
-export([
          in/1, 
          out/1, 
          out_json/2, 
          out_html/4
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
