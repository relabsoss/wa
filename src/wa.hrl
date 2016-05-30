%
% Common project options
%

-define(AFTER(Timeout, Event), {ok, _} = timer:send_after(Timeout, Event)).
-define(ASYNC(F), proc_lib:spawn(fun() -> F end)).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 50, Type, [I]}).
-define(CHILD(I, Type, Param), {I, {I, start_link, Param}, permanent, 50, Type, [I]}).
-define(CHILD(Id, I, Type, Param), {Id, {I, start_link, Param}, permanent, 50, Type, [I]}).

%
% Configuration
%

-define(CONFIG(Key, Default), application:get_env(wa, Key, Default)).
-define(PV(Key, Set), proplists:get_value(Key, Set)).
-define(PV(Key, Set, Default), proplists:get_value(Key, Set, Default)).

%
% Logger
%

-define(ERROR(Msg), lager:error(Msg, [])).
-define(ERROR(Msg, Params), lager:error(Msg, Params)).
-define(INFO(Msg), lager:info(Msg, [])).
-define(INFO(Msg, Params), lager:info(Msg, Params)).
-define(WARNING(Msg), lager:warning(Msg, [])).
-define(WARNING(Msg, Params), lager:warning(Msg, Params)).
-define(DEBUG(Msg), lager:debug(Msg, [])).
-define(DEBUG(Msg, Params), lager:debug(Msg, Params)).

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
-define(IS_SUB(Event), lists:any(fun({P, _}) -> P =:= self() end, gproc:lookup_local_properties(Event))).

%
% Vars
%

-define(STEP, 1000).
-define(RECONNECT_TIMEOUT, 5 * ?STEP).

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

-define(PAYMENT_CHECK, 3 * 60).
-define(PAYMENT_CHECK_MAX, 20).
-define(WAIT_BEFORE_PAY, 1).

-define(MAIL_TEMPLATES, [
    {"reg", template_reg, template_reg_subject},
    {"reset", template_reset, template_reset_subject}
  ]).

