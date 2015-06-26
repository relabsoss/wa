%
% Common project options
%

-define(AFTER(Timeout, Event), {ok, _} = timer:send_after(Timeout, Event)).
-define(ASYNC(F), proc_lib:spawn(fun() -> F end)).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 50, Type, [I]}).
-define(CHILD(I, Type, Param), {I, {I, start_link, Param}, permanent, 50, Type, [I]}).
-define(CHILD(Id, I, Type, Param), {Id, {I, start_link, Param}, permanent, 50, Type, [I]}).

-define(BIN2INT(N), list_to_integer(binary_to_list(N))).

%
% Configuration
%

-define(CONFIG(Key, Default), wa_app:config(Key, Default)).
-define(PV(Key, Set), proplists:get_value(Key, Set)).
-define(PV(Key, Set, Default), proplists:get_value(Key, Set, Default)).

%
% Pub/Sub
%

-define(ME(Reg), gproc:reg({n, l, Reg})).
-define(LOOKUP(Reg), gproc:lookup_pid({n, l, Reg})).
-define(LOOKUPS(Reg), gproc:lookup_pids({n, l, Reg})).
-define(PUB(Event, Msg), pubsub:pub(Event, Msg)).
-define(SUB(Event), pubsub:sub(Event)).
-define(UNSUB(Event), pubsub:unsub(Event)).
-define(LOOKUP_SUB(Reg), gproc:lookup_pids({p, l, Reg})).

%
% Vars
%

-define(RECONNECT_TIMEOUT, 5 * 1000).

%
% Users
%

-define(SIDC, <<"_SID">>).
-define(MAX_FAIL_COUNT, 16).
-define(REAL_IP(Req), cowboy_req:header(<<"X-Real-IP">>, Req, <<"127.0.0.1">>)).

-define(RE_MAIL, "^.+@[^@]+\\.[^@]{2,}$").
-define(RE_TOKEN, "^[a-zA-Z0-9=+\\/]+$").

-define(SHORT_SESSION, 30 * 60).
-define(MEDIUM_SESSION, 24 * 3600).
-define(LONG_SESSION, 30 * 24 * 3600).

-define(S2MS(S), S * 1000).

-define(RECAPTCHA_KEY, <<"">>).

-define(TEMPLATES, [
        {"reg", template_reg},
        {"reset", template_reset}
    ]).
