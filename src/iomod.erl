-module(iomod).

-export([in/1, out/1, out_json/2, out_html/4]).

-include("wa.hrl").
-include_lib("deps/alog/include/alog.hrl").


in(Msg) ->
    case jsonx:decode(Msg, [{format, map}]) of
        {map, Data} -> Data;
        Any ->
            ?INFO("Income very strange ~p => ~p", [Msg, Any]),
            #{}
    end.

out(Msg) ->
    Reply = case jsonx:encode(Msg) of
        Str when is_binary(Str) -> Str;
        Err -> 
            ?ERROR("Error encoding to JSON ~p in ~p", [Err, Msg]), 
            <<"[]">> 
    end,
    {<<"application/json">>, Reply}.
    

out_json(Msg, Req) ->
    {Type, R} = out(Msg),
    {ok, Req1} = cowboy_req:reply(200, [{<<"content-type">>, Type}], R, Req),
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
