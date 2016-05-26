-module(pubsub).
-export([sub/1, pub/2, unsub/1]).

-include_lib("wa.hrl").

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
