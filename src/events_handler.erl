-module(events_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3, websocket_init/3, websocket_handle/3, websocket_info/3, websocket_terminate/3]).

-include("wa.hrl").
-include_lib("deps/alog/include/alog.hrl").


init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    {ok, Req1, Pid} = session:check(Req),
    {ok, Req, #{ session => Pid }}.


websocket_handle({text, Msg}, Req, State) ->
    NewState = process(iomod:in(Msg), State),
    {ok, Req, NewState};

websocket_handle(Data, Req, State) ->
    ?INFO("Unknown data handled: ~p", [Data]),
    {ok, Req, State}.


websocket_info({send, Msg}, Req, State) ->
    {_, Reply} = iomod:out(Msg),
    {reply, {text, Reply}, Req, State};

websocket_info(Info, Req, State) ->
    ?INFO("Unknown info handled: ~p", [Info]),
    {ok, Req, State}.


websocket_terminate(_Reason, _Req, _State) ->
    ok.

% default

process(Msg, State) -> 
    ?INFO("Unknown message ~p", [Msg]), 
    State. 
