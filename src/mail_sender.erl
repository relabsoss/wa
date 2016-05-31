-module(mail_sender).
-behaviour(gen_server).
-export([
          mail/3,
          start_link/1, 
          init/1, 
          handle_call/3, 
          handle_cast/2, 
          handle_info/2, 
          terminate/2, 
          code_change/3
        ]).

-include("wa.hrl").

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
      {ok, _} = erlydtl:compile_file(
        filename:join([Priv, "templates", "mail", Fn ++ ".dtl"]), 
        Module, [{auto_escape, false}]),
      {ok, _} = erlydtl:compile_file(
        filename:join([Priv, "templates", "mail", "subject", Fn ++ ".dtl"]), 
        SModule, [{auto_escape, false}]),
      {Module, SModule}
    end, ?MAIL_TEMPLATES),
  {ok, Params#{ subjects => Subjects }}.

%
% gen_server
%

handle_call(_Msg, _From, State) -> 
  {reply, ok, State}.

handle_cast({mail, To, Template, Params}, #{ 
        domain := Domain, 
        from := From,
        subjects := Subjects,
        method := Method, 
        options := Options 
      } = State) ->
  MParams = Params ++ [{mail_to, To}, {domain, Domain}],

  STemplate = proplists:get_value(Template, Subjects),
  {ok, USubject} = STemplate:render(MParams),
  Subject = iolist_to_binary(USubject),
  {ok, UBody} = Template:render(MParams),
  Body = iolist_to_binary(UBody),
  
  ?MODULE:Method(From, To, Subject, Body, Options),

  {noreply, State};
handle_cast(_Msg, State) -> 
  {noreply, State}.

handle_info(_Info, State) -> 
  {noreply, State}.

terminate(_Reason, _State) -> 
  ok.

code_change(_OldVsn, State, _Extra) -> 
  {ok, State}.

%
% methods
%

mailgun(From, To, Subject, Body, #{ url := URL, priv_key := PrivKey }) ->  
  Base64 = base64:encode(<<"api:", PrivKey/binary>>),
  case restc:request(post, qs, URL, [200], 
      #{
        <<"Authorization">> => <<"Basic ", Base64/binary>>
      }, 
      #{
        <<"from">> => From,
        <<"to">> => To,
        <<"subject">> => Subject,
        <<"text">> => Body
      }) of
    {ok, _, _, Resp} ->
      ?INFO("Send mail to ~p with response ~p", [To, Resp]);
    Error ->
      ?ERROR("Can't send mail to ~p - ~p", [To, Error])
  end.

smtp(From, To, Subject, Body, Options) ->
  gen_smtp_client:send({
    From, 
    [To], 
    <<"Subject: ", Subject/binary, "/r/n",
      "From: ", From/binary, "/r/n",
      "To: ", To/binary, "/r/n",
      "/r/n", Body>>}, maps:to_list(Options)).  
