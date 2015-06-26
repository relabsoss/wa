-module(pubsub).

-export([sub/1, pub/2, unsub/1]).

-include_lib("deps/alog/include/alog.hrl").

-define(IS_SUB(Event), lists:any(fun({P, _}) -> P =:= self() end, gproc:lookup_local_properties(Event))).


sub(Event) -> 
    case ?IS_SUB(Event) of
        true ->
            true;
        false ->
            gproc:reg({p, l, Event})
    end.


pub(Event, Msg) ->
    gproc:send({p, l, Event}, Msg).


unsub(Event) ->
    case ?IS_SUB(Event) of
        true ->
            gproc:unreg({p, l, Event});
        false ->
            true
    end.
