-module(persist_pgsql).
-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([q/2, q/3, qi/2, ql/2, qs/2, qe/3, field/3, field/4]).
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("wa.hrl").

-include_lib("deps/alog/include/alog.hrl").
-include_lib("deps/epgsql/include/epgsql.hrl").

%
% external
%
 
start_link(Params) -> 
    gen_server:start_link(?MODULE, Params, []).


init([Params]) ->
    process_flag(trap_exit, true),
    ?AFTER(0, reconnect),
    {ok, {undefine, Params}}.

q(Pool, Query) -> 
    poolboy:transaction(Pool, fun(Worker) ->
            gen_server:call(Worker, {q, Query})
        end).   

q(Pool, Query, Timeout) -> 
    poolboy:transaction(Pool, fun(Worker) ->
            gen_server:call(Worker, {q, Query}, Timeout)
        end).   

qi(Pool, Query) -> q(Pool, Query, infinity).

ql(Pool, QueryList) when is_list(QueryList) ->
    [q(Pool, Q) || Q <- QueryList]. 

qs(Pool, Query) ->
    case persist_pgsql:qi(Pool, Query) of
        {ok, _} -> ok;
        Error -> ?ERROR("Error ~p", [Error]), error
    end.

qe(Pool, Query, Params) -> 
    poolboy:transaction(Pool, fun(Worker) ->
            gen_server:call(Worker, {qe, Query, Params})
        end).   

field(Row, Column, Columns) ->
    ColumnB = list_to_binary(Column),
    lists:filter(fun({C, _}) -> C#column.name == ColumnB end, 
        lists:zip(Columns, tuple_to_list(Row))).

field(Row, Column, Columns, Value) ->
    ValueB = list_to_binary(Value),
    case field(Row, Column, Columns) of
        [{_, ValueB}] -> true;
        _ -> false
    end.

%
% gen_server
%

handle_call({q, _Query}, _From, {undefine, _} = State) -> 
    {reply, {error, noconnection}, State};

handle_call({q, Query}, _From, {C, _} = State) -> 
    case epgsql:squery(C, Query) of
        {ok, Count} -> {reply, {ok, Count}, State};
        {ok, Columns, Rows} -> {reply, {ok, {Columns, Rows}}, State};
        {ok, _Count, Columns, Rows} -> {reply, {ok, {Columns, Rows}}, State};
        Err -> ?ERROR("PostgreSQL error ~p - ~p", [Query, Err]), {reply, {error, Err}, State}
    end;

handle_call({qe, _Query, _Params}, _From, {undefine, _} = State) -> 
    {reply, {error, noconnection}, State};

handle_call({qe, Query, Params}, _From, {C, _} = State) -> 
    case epgsql:equery(C, Query, Params) of
        {ok, Count} -> {reply, {ok, Count}, State};
        {ok, Columns, Rows} -> {reply, {ok, {Columns, Rows}}, State};
        {ok, _Count, Columns, Rows} -> {reply, {ok, {Columns, Rows}}, State};
        Err -> ?ERROR("PostgreSQL error ~p - ~p", [Query, Err]), {reply, {error, Err}, State}
    end;

handle_call(_Msg, _From, State) -> 
    {reply, ok, State}.


handle_cast(_Msg, State) -> 
    {noreply, State}.


handle_info(reconnect, {undefine, []} = State) -> 
    {noreply, State};

handle_info(reconnect, {undefine, Params}) -> 
    try_connect(Params);

handle_info({'EXIT', C, Reason}, {C, Params}) ->
    ?INFO("PgSQL connection dead ~p", [C]),
    reconnect(Reason, Params);

handle_info(_Info, State) -> 
    {noreply, State}.


terminate(_Reason, {undefine, _}) -> 
    ok;

terminate(_Reason, {C, _}) -> 
    ?INFO("PgSQL connection down ~p", [C]),
    epgsql:close(C),
    ok.


code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.

%
% misc
%

try_connect(Params) ->
    Host = proplists:get_value(host, Params),
    Port = proplists:get_value(port, Params, 5432),
    User = proplists:get_value(user, Params),
    Password = proplists:get_value(password, Params),
    Database = proplists:get_value(db, Params),
    case epgsql:connect(Host, User, Password, 
            [{port, Port}, 
             {database, Database},  
             {timeout, infinity}]) of
        {ok, C} -> 
            ?INFO("PgSQL connection up ~p", [C]), 
            {noreply, {C, Params}};
        Reason -> 
            reconnect(Reason, Params)
    end.

reconnect(Reason, Params) ->
    ?ERROR("Error in connection to PgSQL server - ~p", [Reason]),
    ?AFTER(?RECONNECT_TIMEOUT, reconnect),
    {noreply, {undefine, Params}}.
