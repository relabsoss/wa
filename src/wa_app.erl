-module(wa_app).
-behaviour(application).

-export([start/2, stop/1]).
-export([config/2, priv_dir/0]).

start(_StartType, _StartArgs) ->
    Priv = priv_dir(),
    VRoutes = [
            {"/", cowboy_static, {file, filename:join([Priv, "www", "index.html"])}},
            {"/oauth/:provider/:action", social_handler, config(social, [])},    
            {"/auth/[...]", auth_handler, []},
            {"/events", events_handler, []},
            {"/[...]", cowboy_static, {dir,  filename:join(Priv, "www")}}
        ],
    Dispatch = cowboy_router:compile([{'_',  VRoutes}]),
    cowboy:start_http(webapp_http_listener, 5, 
                      [{port, 8080}],
                      [{env, [{dispatch, Dispatch}]}]),
    wa_sup:start_link().

stop(_State) ->
    ok.

config(Key, Default) ->
    case application:get_env(Key) of
        {ok, Value} -> Value;
        _ -> Default
    end.

priv_dir() ->
    Ebin = filename:dirname(code:which(?MODULE)),
    filename:join(filename:dirname(Ebin), "priv").
