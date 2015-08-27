-module(session).
-behaviour(gen_server).

-export([check/1, process/3, random/0, pwd_to_db_pwd/1, smd5/1]).
-export([social/3, current/2, user_info/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("wa.hrl").
-include_lib("deps/alog/include/alog.hrl").


check(Req) ->
    {SID, Req1} = cowboy_req:cookie(?SIDC, Req, undefined),
    try ?LOOKUP(SID) of 
        Pid ->
            Expire = gen_server:call(Pid, timer),
            Req2 = cowboy_req:set_resp_cookie(?SIDC, SID, [{max_age, Expire}, {path, "/"}], Req1),
            {ok, Req2, Pid}
    catch 
        _:_ ->
            NewSID = random(),
            {ok, Pid} = start([NewSID]),
            Req2 = cowboy_req:set_resp_cookie(?SIDC, NewSID, [{max_age, ?SHORT_SESSION}, {path, "/"}], Req1),
            {ok, Req2, Pid}
    end.

process(Pid, Path, Req) ->
    gen_server:call(Pid, {process, Path, Req}).

social(Provider, Props, Req) ->
    {ok, Req1, Pid} = check(Req),
    gen_server:call(Pid, {social, Provider, Props, Req1}).

current(Pid, URL) ->
    gen_server:cast(Pid, {current, URL}).

user_info(Pid) ->
    gen_server:call(Pid, user_info).

    
%
% external
%
 
start(Params) -> 
    ?DEBUG("Create session with ~p", [Params]),
    gen_server:start(?MODULE, Params, []).


init([SID]) ->
    ?ME(SID),
    {ok, Timer} = timer:kill_after(?S2MS(?SHORT_SESSION)),
    {ok, #{ 
        sid => SID, 
        time_to_die => Timer, 
        timer => ?SHORT_SESSION,
        auth => false,
        mail => undefined,
        info => #{},
        fail_count => 0,
        token => <<>>,
        current_page => <<"/">> }}.

%
% gen_server
%

handle_call(user_info, _From, State) ->
    {reply, #{mail => maps:get(mail, State),
              auth => maps:get(auth, State),
              info => maps:get(info, State)}, State};

handle_call(info, _From, State) ->
    {ok, cancel} = timer:cancel(maps:get(time_to_die, State)),
    {ok, Timer} = timer:kill_after(?S2MS(maps:get(timer, State))),    
    {reply, {maps:get(mail, State), maps:get(info, State)}, State#{ time_to_die := Timer }};

handle_call(timer, _From, State) ->
    {ok, cancel} = timer:cancel(maps:get(time_to_die, State)),
    {ok, Timer} = timer:kill_after(?S2MS(maps:get(timer, State))),    
    {reply, maps:get(timer, State), State#{ time_to_die := Timer }};
    

handle_call({process, Path, Req}, _From, State) ->
    {ok, cancel} = timer:cancel(maps:get(time_to_die, State)),
    {Reply, NewReq, NewState} = pass(maps:get(auth, State), Path, Req, State),
    {ok, Timer} = timer:kill_after(?S2MS(maps:get(timer, NewState))),    
    {reply, {Reply, NewReq}, NewState#{ time_to_die := Timer }};

handle_call({social, Provider, Props, Req}, _From, State) ->
    {ok, cancel} = timer:cancel(maps:get(time_to_die, State)),
    {NewReq, NewState} = pass_social(Provider, Props, Req, State),
    {ok, Timer} = timer:kill_after(?S2MS(maps:get(timer, NewState))),    
    {reply, NewReq, NewState#{ time_to_die := Timer }};

handle_call(_Msg, _From, State) -> 
    {reply, ok, State}.


handle_cast({current, URL}, State) ->
    {ok, cancel} = timer:cancel(maps:get(time_to_die, State)),
    {ok, Timer} = timer:kill_after(?S2MS(maps:get(timer, State))),    
    {noreply, State#{ current_page := URL, time_to_die := Timer }};

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%
% pass
%

-define(ER(Error), [{result, error}, {error, Error}]).

pass(false, [<<"user">>, <<"info">>], Req, State) ->
    {?ER(unknown_user), Req, State};

pass(false, [<<"login">>], Req, State) ->
    case cowboy_req:body_qs(Req) of 
        {ok, Values, Req1} ->
            Mail = proplists:get_value(<<"mail">>, Values, <<"">>),
            Pwd = proplists:get_value(<<"pass">>, Values, <<"">>),
            DBPwd = plain_pwd_to_db_pwd(Pwd),
            case persist:get_user(pgdb, Mail, DBPwd) of
                {M, FN, LN} ->
                    ?SUB(M),
                    {[{result, ok}], Req1, State#{ 
                            auth := true, 
                            mail := M, 
                            info := #{fname => FN, lname => LN}, 
                            timer := ?LONG_SESSION 
                        }};
                _ ->
                    FC = maps:get(fail_count, State) + 1,
                    case FC > ?MAX_FAIL_COUNT of
                        true ->
                            {IP, Req2} = ?REAL_IP(Req1), 
                            ?INFO("User with IP ~p is look like c00l hAtzker! ~pth auth try.", [IP, FC]),
                            {?ER(unknown_user_really), Req2, State#{ fail_count := FC }};
                        false ->
                            {?ER(unknown_user), Req1, State#{ fail_count := FC }}
                    end
            end;
        Any ->
            ?ERROR("Passing params reply ~p", [Any]),
            {?ER(operation_failed), Req, State}
    end;

pass(false, [<<"reg">>], Req, State) ->
    case cowboy_req:body_qs(Req) of 
        {ok, Values, Req1} ->
            Mail = proplists:get_value(<<"mail">>, Values, <<"">>),
            FName = proplists:get_value(<<"fname">>, Values, <<"">>),
            LName = proplists:get_value(<<"lname">>, Values, <<"">>),
            case (re:run(Mail, ?RE_MAIL) =:= nomatch) or (byte_size(FName) =:= 0) or (byte_size(LName) =:= 0) of
                true ->
                    {?ER(bad_income), Req1, State};
                false ->
                    {IP, Req2} = ?REAL_IP(Req),
                    case recaptcha:check(IP, Values, ?CONFIG(recaptcha_key, <<"">>)) of
                        false ->
                            {?ER(captcha_fail), Req2, State};
                        true ->
                            case persist:check_user(pgdb, Mail) of
                                true ->
                                    {?ER(already), Req2, State};    
                                false ->
                                    SMail = strip(Mail),
                                    SFName = strip(FName),
                                    SLName = strip(LName),
                                    Token = random(),
                                    ?SUB(Token),
                                    mail_sender:mail(SMail, template_reg, [{token, Token}]),
                                    {[{result, ok}], Req2, State#{ 
                                            mail := SMail, 
                                            token := Token, 
                                            info := #{fname => SFName, lname => SLName}, 
                                            timer := ?MEDIUM_SESSION 
                                        }}
                            end
                    end
            end;
        Any ->
            ?ERROR("Passing params reply ~p", [Any]),
            {?ER(operation_failed), Req, State}
    end;

        
pass(false, [<<"reset">>], Req, State) ->
    case cowboy_req:body_qs(Req) of 
        {ok, Values, Req1} ->
            Mail = proplists:get_value(<<"mail">>, Values, <<"">>),
            case re:run(Mail, ?RE_MAIL) =:= nomatch of
                true ->
                    {?ER(bad_income), Req1, State};
                false ->
                    {IP, Req2} = ?REAL_IP(Req),
                    case recaptcha:check(IP, Values, ?CONFIG(recaptcha_key, <<"">>)) of
                        false ->
                            {?ER(captcha_fail), Req2, State};
                        true ->
                            case persist:check_user(pgdb, Mail) of
                                true ->
                                    SMail = strip(Mail),
                                    Token = random(),
                                    ?SUB(Token),
                                    mail_sender:mail(SMail, template_reset, [{token, Token}]),
                                    {[{result, ok}], Req2, State#{ 
                                            mail := SMail, 
                                            token := Token, 
                                            timer := ?MEDIUM_SESSION 
                                        }};
                                false ->
                                    {?ER(unknown_user), Req2, State}                         
                            end
                    end
            end;
        Any ->
            ?ERROR("Passing params reply ~p", [Any]),
            {?ER(operation_failed), Req, State}
    end;

pass(false, [<<"update">>], Req, State) ->
    case cowboy_req:body_qs(Req) of 
        {ok, Values, Req1} ->
            Pwd = proplists:get_value(<<"pass">>, Values, <<"">>),
            Token = proplists:get_value(<<"token">>, Values, <<"">>),    
            case (byte_size(Pwd) =:= 0) or (re:run(Token, ?RE_TOKEN) =:= nomatch) of
                true ->
                    {?ER(bad_income), Req1, State};
                false ->
                    try ?LOOKUP_SUB(Token) of 
                        [Pid] when Pid =:= self() ->
                            ?UNSUB(Token),
                            update_password(maps:get(mail, State), maps:get(info, State), Pwd, Req1, State);                            
                        [Pid] when Pid =/= self() ->
                            {Mail, Info} = gen_server:call(Pid, info),
                            timer:kill_after(0, Pid),
                            update_password(Mail, Info, Pwd, Req1, State);
                        Any ->
                            ?DEBUG("Invalid token ~p - reply ~p", [Token, Any]),
                            {?ER(invalid_token), Req1, State}                
                    catch 
                        error:Error ->
                            ?DEBUG("Invalid token - error ~p", [Error]),
                            {?ER(invalid_token), Req1, State}
                    end
            end;
        Any ->
            ?ERROR("Passing params reply ~p", [Any]),
            {?ER(operation_failed), Req, State}
    end;

pass(true, [<<"user">>, <<"info">>], Req, #{ mail := Mail, info := Info } = State) ->
    Token = random(),
    {social, Socials} = socials_out(persist:list_social_link(pgdb, Mail)), 
    R = #{result => ok, mail => Mail, token => Token, info => Info, social => Socials},
    {R, Req, State#{ token := Token }};

pass(true, [<<"update">>], Req, State) ->
    check_token(Req, State, fun(Req1, Values) ->
            Pwd = proplists:get_value(<<"pass">>, Values, <<"123">>),
            DBPwd = plain_pwd_to_db_pwd(Pwd),
            case persist:update_user(pgdb, maps:get(mail, State), DBPwd) of
                {ok, 1} ->
                    {[{result, ok}], Req1, State#{ token := <<"">> }};
                _ ->
                    {?ER(operation_failed), Req1, State#{ token := <<"">> }}
            end        
        end);

pass(true, [<<"unbind">>], Req, State) ->
    check_token(Req, State, fun(Req1, Values) ->
            SocId = proplists:get_value(<<"socid">>, Values, <<"">>),
            persist:del_social_link(pgdb, SocId),
            {[{result, ok}], Req1, State#{ token := <<"">> }}
        end);

pass(true, [<<"logout">>], Req, State) ->
    {[{result, ok}], Req, State#{ 
            auth := false, 
            mail := undefinded, 
            info := #{}, 
            fail_count := 0, 
            timer := ?SHORT_SESSION
        }};

pass(_Auth, Path, Req, State) ->
    ?INFO("Unknown request ~p for session ~p", [Path, State]),
    {?ER(unknown_request), Req, State}.


update_password(Mail, Info, Pwd, Req, State) ->
    case Mail of
        undefined ->
            {?ER(unknown_user), Req, State};
        _ ->
            DBPwd = plain_pwd_to_db_pwd(Pwd),  
            case persist:check_user(pgdb, Mail) of
                true ->
                    case persist:update_user(pgdb, Mail, DBPwd) of
                        {ok, 1} ->
                            case persist:get_user(pgdb, Mail, DBPwd) of
                                {M, FN, LN} ->
                                    ?SUB(M),
                                    {[{result, ok}], Req, State#{ 
                                            auth := true, 
                                            mail := M, 
                                            info := #{fname => FN, lname => LN}, 
                                            timer := ?LONG_SESSION 
                                        }};
                                _ ->
                                    {?ER(operation_failed), Req, State#{ token := <<"">> }}    
                            end;
                        _ ->
                            {?ER(operation_failed), Req, State#{ token := <<"">> }}
                    end;
                false ->
                    FName = proplists:get_value(fname, Info, <<"Guest">>),
                    LName = proplists:get_value(lname, Info, <<"">>),
                    case persist:add_user(pgdb, Mail, FName, LName, DBPwd) of
                        {ok, 1} ->
                            ?SUB(Mail),
                            {[{result, ok}], Req, State#{ 
                                    auth := true, 
                                    mail := Mail, 
                                    info := Info, 
                                    timer := ?LONG_SESSION, 
                                    token := <<>> 
                                }};
                        _ ->
                            {?ER(operation_failed), Req, State#{ token := <<"">> }}
                    end
            end
    end.

check_token(Req, State, Fun) ->
    case cowboy_req:body_qs(Req) of 
        {ok, Values, Req1} ->
            Token = proplists:get_value(<<"token">>, Values, <<"">>),    
            case (byte_size(Token) =/= 0) and (Token =:= maps:get(token, State))  of
                true -> Fun(Req1, Values);        
                false -> {?ER(invalid_token), Req1, State#{ token := <<"">> }}
            end;
        Any ->
            ?ERROR("Passing params reply ~p", [Any]),
            {?ER(operation_failed), Req, State}
    end.

%
% social
%

pass_social(Provider, Props, Req, State) ->
    A = maps:get(auth, State),
    {URL, NewState} = case social:fetch(Provider, Props) of
        {ok, UserInfo} when A ->
            % link
            Mail = persist:has_social_link(pgdb, maps:get(id, UserInfo)),
            Op = case Mail of
                none -> add_social_link;
                _ -> update_social_link
            end,
            persist:Op(pgdb, maps:get(mail, State), UserInfo),
            {maps:get(current_page, State), State};
        {ok, UserInfo} when not(A) ->
            % login || reg
            UpState = case persist:has_social_link(pgdb, maps:get(id, UserInfo)) of
                none ->
                    Mail = maps:get(mail, UserInfo),
                    case persist:get_user(pgdb, Mail) of
                        none ->
                            case persist:add_user(pgdb, Mail, maps:get(fname, UserInfo), 
                                    maps:get(lname, UserInfo), "none") of
                                {ok, 1} ->
                                    ?SUB(Mail),
                                    persist:add_social_link(pgdb, Mail, UserInfo),
                                    State#{ 
                                        auth := true,
                                        mail := Mail, 
                                        info := #{fname => maps:get(fname, UserInfo), 
                                                  lname => maps:get(lname, UserInfo)},
                                        timer := ?LONG_SESSION };
                                _ ->
                                    State
                            end;    
                        {_, FName, LName} ->
                            ?SUB(Mail),
                            persist:add_social_link(pgdb, Mail, UserInfo),
                            State#{ 
                                auth := true,
                                mail := Mail, 
                                info := #{fname => FName, lname => LName},
                                timer := ?LONG_SESSION }
                    end;
                Mail ->    
                    case persist:get_user(pgdb, Mail) of
                        {_, FName, LName} ->            
                            ?SUB(Mail),
                            persist:update_social_link(pgdb, Mail, UserInfo),
                            State#{ 
                                auth := true,
                                mail := Mail, 
                                info := #{fname => FName, lname => LName},
                                timer := ?LONG_SESSION };
                        _ ->
                            State
                    end
            end,
            {maps:get(current_page, UpState), UpState};
        Error ->
            ?ERROR("Error in fetching social data ~p", [Error]),
            {<<"/">>, State}
    end,
    {ok, Req1} = cowboy_req:reply(302, [{<<"location">>, URL}], <<>>, Req),
    {Req1, NewState}.

socials_out(List) ->
    {social, [[{id, Id}, {title, Title}]|| {Id, Title, _} <- List]}.

%
% local
%

strip(S) -> 
    re:replace(S, "[<>&/]+", "", [global,{return, binary}]).

pwd_to_db_pwd(MD5Bin) ->
    smd5(<<MD5Bin/binary, (?CONFIG(salt, <<"deadbeef">>))/binary>>).

plain_pwd_to_db_pwd(Bin) when is_binary(Bin) ->
    plain_pwd_to_db_pwd(binary_to_list(Bin));

plain_pwd_to_db_pwd(L) ->
    pwd_to_db_pwd(list_to_binary(smd5(L))).

smd5(S) ->
    lists:flatten([io_lib:format("~2.16.0b", [C]) || <<C>> <= erlang:md5(S)]).

random() ->
    base64:encode(crypto:strong_rand_bytes(?CONFIG(sid_size, 64))).
