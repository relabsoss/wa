-module(iomod).

-export([in/1, out/1]).

-include("wa.hrl").
-include_lib("deps/alog/include/alog.hrl").


in(Msg) ->
    case jsonx:decode(Msg, [{format, map}]) of
        {error, _Error} -> #{};
        {error, _Error, _Str} -> #{};
        Data -> Data
    end.

out(Msg) ->
    Reply = case jsonx:encode(Msg) of
        Str when is_binary(Str) -> Str;
        Err -> 
            ?ERROR("Error encoding to JSON ~p in ~p", [Err, Msg]), 
            <<"[]">> 
    end,
    {<<"application/json">>, Reply}.
    