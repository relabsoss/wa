-module(mail_sender).
-behaviour(gen_server).

-export([mail/3]).
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("wa.hrl").
-include_lib("deps/alog/include/alog.hrl").

mail(To, Template, Params) ->
    gen_server:cast(?MODULE, {mail, To, Template, Params}).

%
% external
%
 
start_link(Params) -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, Params, []).


init(Params) ->
    Priv = wa_app:priv_dir(),
    lists:map(fun({Fn, Module}) -> 
            File = lists:concat([Priv, "/templates/mail/", Fn, ".dtl"]),
            {ok, _} = erlydtl:compile_file(File, Module, [{auto_escape, false}])
        end, ?TEMPLATES),
    {ok, Params}.

%
% gen_server
%

handle_call(_Msg, _From, State) -> 
    {reply, ok, State}.


handle_cast({mail, To, Template, Params}, State) ->
    {ok, UBody} = Template:render(Params ++ [{mail_to, To}, {domain, maps:get(domain, State)}]),
    Body = binary_to_list(iolist_to_binary(UBody)),

    % TODO: send via MailGun

    {noreply, State};

handle_cast(_Msg, State) -> 
    {noreply, State}.

handle_info(_Info, State) -> 
    {noreply, State}.


terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
