-module(restc).
-export([request/6]).

request(Method, Type, Url, Expect, Headers, Body) ->
    Headers1 = [{"Accept", get_accesstype(Type)++", */*;q=0.9"} | Headers],
    Headers2 = [{"Content-Type", get_ctype(Type)} | Headers1],
    Response = process(Method, Url, Type, Headers2, Body),
    case Response of
        {ok, Status, H, B} ->
            case check_expect(Status, Expect) of
                true -> Response;
                false -> {error, Status, H, B}
            end;
        Error ->
            Error
    end.

process(Method, Url, Type, Headers, Body) ->
    {ok, {_, _, Host, Port, Path, QS}} = http_uri:parse(Url),
    {ok, ConnPid} = gun:open(Host, Port, #{ protocols => [http] }),
    {ok, _Protocol} = gun:await_up(ConnPid),
    StreamRef = case lists:any(fun(I) -> I =:= Method end, [post, put]) of  
        true -> 
            EncBody = encode_body(Type, Body),
            gun:Method(ConnPid, Path ++ QS, Headers ++ ["content-length", byte_size(EncBody)], EncBody);
        false when is_list(Body) and (Body =/= []) and (length(QS) =:= 0) ->
            gun:Method(ConnPid, lists:concat([Path, "?", binary_to_list(cow_qs:qs(Body))]), Headers);
        false when is_list(Body) and (Body =/= []) and (length(QS) =/= 0) ->
            gun:Method(ConnPid, lists:concat([Path, QS, "&", binary_to_list(cow_qs:qs(Body))]), Headers);
        false -> 
            gun:Method(ConnPid, Path ++ QS, Headers)
    end,
    Resp = case gun:await(ConnPid, StreamRef) of
        {response, fin, Status, RespHeaders} ->
            {ok, Status, RespHeaders, []};
        {response, nofin, Status, RespHeaders} ->
            case gun:await_body(ConnPid, StreamRef) of
                {ok, RespBody} -> 
                    {ok, Status, RespHeaders, parse(RespHeaders, RespBody)};               
                Error ->
                    {error, Error}
            end;
        Any -> Any
    end,
    gun:shutdown(ConnPid),
    Resp.

check_expect(_Status, []) ->
    true;
check_expect(Status, Expect) ->
    lists:member(Status, Expect).

encode_body(json, Body)    -> jsonx:encode(Body);
encode_body(percent, Body) -> cow_qs:qs(Body);
encode_body(html, Body)    -> Body;
encode_body(_, Body)       -> encode_body(?DEFAULT_ENCODING, Body).

parse(Headers, Body) ->
    Unzip = case proplists:get_value(<<"content-encoding">>, Headers, <<"plain">>) of
        <<"gzip">> -> zlib:gunzip(Body);
        _ -> Body
    end,
    Type = case lists:keyfind(<<"content-type">>, 1, Headers) of
        false    -> ?DEFAULT_CTYPE;
        {_, Val} -> Val
    end,
    CType = case string:tokens(binary_to_list(Type), ";") of
        [CVal]    -> CVal;
        [CVal, _] -> CVal
    end,
    parse_body(CType, Unzip).

parse_body([], Body)                 -> Body;
parse_body(_, [])                    -> [];
parse_body(_, <<>>)                  -> [];
parse_body("text/html", Body)        -> Body;
parse_body("application/json", Body) -> 
    case jsx:decode(Body, [{format, proplist}]) of
        {error, _Reason} -> [];
        {error, _Reason, _Line} -> [];
        Any -> Any
    end; 
parse_body("application/x-www-form-urlencoded", Body) -> cow_qs:parse_qs(Body);
parse_body("text/plain", Body) -> cow_qs:parse_qs(Body); % damn you, Facebook
parse_body(_, Body)                  -> Body.

get_accesstype(json)    -> "application/json";
get_accesstype(percent) -> "application/json";
get_accesstype(html)    -> "text/html";
get_accesstype(_)       -> get_ctype(?DEFAULT_ENCODING).

get_ctype(json)    -> "application/json";
get_ctype(percent) -> "application/x-www-form-urlencoded";
get_ctype(html)    -> "text/html";
get_ctype(_)       -> get_ctype(?DEFAULT_ENCODING).
