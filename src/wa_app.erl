-module(wa_app).
-behaviour(application).

-export([
        start/2, 
        stop/1,
        priv_dir/0
    ]).

-include("wa.hrl").

start(_StartType, _StartArgs) ->
  Priv = priv_dir(),
  VRoutes = [
      {"/", pi_handler, index},
      {"/oauth/:provider/:action", social_handler, ?CONFIG(social, [])},    
      {"/auth/[...]", auth_handler, []},
      {"/events", events_handler, []},
      {"/paypal/[...]", paypal_handler, ?CONFIG(payments, #{})},
      {"/[...]", cowboy_static, {dir,  filename:join(Priv, "www")}}
    ],
  Dispatch = cowboy_router:compile([{'_',  VRoutes}]),
  compile_templates(Priv),
  cowboy:start_http(webapp_http_listener, 5, 
                    [{port, 8080}],
                    [{env, [{dispatch, Dispatch}]}]),
  wa_sup:start_link().

stop(_State) ->
  ok.

priv_dir() ->
  Ebin = filename:dirname(code:which(?MODULE)),
  filename:join(filename:dirname(Ebin), "priv").

compile_templates(Priv) ->
  lists:map(fun(Dir) -> 
      {ok, Files} = file:list_dir(Dir),
      ValidFiles = lists:filter(fun(I) -> filename:extension(I) =:= ".dtl" end, Files),
      R = [{F, erlydtl:compile_file(
          Dir ++ F, 
          list_to_atom(re:replace(F, "\\.", "_", [{return, list}])), 
          [{auto_escape, false}])} || F <- ValidFiles],
      ?INFO("Compile web dtl's - ~p", [R])
    end, [
          filename:join([Priv, "templates", "www"])
          ]).

