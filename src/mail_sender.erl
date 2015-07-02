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
    Subjects = lists:map(fun({Fn, Module, SModule}) -> 
            File = lists:concat([Priv, "/templates/mail/", Fn, ".dtl"]),
            {ok, _} = erlydtl:compile_file(File, Module, [{auto_escape, false}]),
            SFile = lists:concat([Priv, "/templates/mail/subject/", Fn, ".dtl"]),
            {ok, _} = erlydtl:compile_file(SFile, SModule, [{auto_escape, false}]),
            {Module, SModule}
        end, ?TEMPLATES),
    {ok, Params#{ subjects => Subjects }}.

%
% gen_server
%

handle_call(_Msg, _From, State) -> 
    {reply, ok, State}.


handle_cast({mail, To, Template, Params}, State) ->
    URL = binary_to_list(maps:get(url, State)),
    Domain = maps:get(domain, State),
    From = maps:get(from, State),
    MParams = Params ++ [{mail_to, To}, {domain, Domain}],

    STemplate = proplists:get_value(Template, maps:get(subjects, State)),
    {ok, USubject} = STemplate:render(MParams),
    Subject = iolist_to_binary(USubject),
    
    {ok, UBody} = Template:render(MParams),
    Body = iolist_to_binary(UBody),
    
    Base64 = base64:encode("api:" ++ binary_to_list(maps:get(priv_key, State))),
    case restc:request(post, percent, URL, [200], 
            [{<<"Authorization">>, <<"Basic ", Base64/binary>>}], 
            [
                {<<"from">>, From},
                {<<"to">>, To},
                {<<"subject">>, Subject},
                {<<"text">>, Body} 
             ]) of
        {ok, _, _, Resp} ->
            ?INFO("Send mail to ~p with response ~p", [To, Resp]);
        Error ->
            ?ERROR("Can't send mail to ~p - ~p", [To, Error])
    end,
    {noreply, State};

handle_cast(_Msg, State) -> 
    {noreply, State}.

handle_info(_Info, State) -> 
    {noreply, State}.


terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
