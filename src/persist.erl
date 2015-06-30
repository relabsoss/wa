-module(persist).

-export([init_db/1, init_db/2]).
-export([check_user/2, get_user/3, add_user/5, update_user/3, del_user/2]).
-export([]).

-include_lib("deps/alog/include/alog.hrl").

-define(OR_EMPTY(Action), case Action of {ok, {_, Strs}} -> Strs; Any -> ?ERROR("~p", [Any]), [] end).

%
% init db ( persist:init_db(pgdb). )
%

init_db(Pool) ->
    [init_db(I, Pool) || I <- [1, 2]].

init_db(1, Pool) ->
    persist_pgsql:ql(Pool, [
            %
            % Users
            %
            "CREATE TABLE users (
                    mail varchar(128) PRIMARY KEY,
                    pass varchar(64) NOT NULL,
                    fname varchar(128),
                    lname varchar(128),
                    en boolean NOT NULL DEFAULT TRUE,
                    atime timestamp DEFAULT current_timestamp
                );",
            "CREATE INDEX users_mail_en_idx ON users(mail, en);",
            "CREATE INDEX users_mail_pass_en_idx ON users(mail, pass, en);"
         ]);
init_db(2, Pool) ->
    persist_pgsql:ql(Pool, [
            %
            % Social
            %
            "CREATE TABLE users_social (
                    id varchar(256) PRIMARY KEY,
                    mail varchar(128),
                    title varchar(256),
                    token varchar(256),
                    en boolean NOT NULL DEFAULT TRUE,
                    atime timestamp DEFAULT current_timestamp
                );",
            "CREATE INDEX users_social_mail_idx ON users_social(mail, en);",
            "CREATE INDEX users_social_id_mail_en_idx ON users_social(id, mail, en);"
         ]).


%
% user's ops
%

check_user(Pool, Mail) ->
    case persist_pgsql:qe(Pool, "SELECT COUNT(mail) AS c FROM users WHERE en=TRUE AND mail=$1", [Mail]) of
        {ok, {_, [{Count}]}} when Count =:= 1 ->
            ?INFO("Check for user ~p successful.", [Mail]), 
            true;
        Any -> 
            ?WARNING("Check for user ~p fail (reply ~p)", [Mail, Any]),
            false
    end.


get_user(Pool, Mail, Pass) ->
    case persist_pgsql:qe(Pool, "SELECT mail, fname, lname FROM users WHERE en=TRUE AND mail=$1 AND pass=$2", [Mail, Pass]) of
        {ok, {_, [{Mail, FName, LName}]}} ->
            ?INFO("Auth for user ~p successful.", [Mail]), 
            {Mail, FName, LName};
        Any -> 
            ?WARNING("Auth for user ~p fail (pass ~p, reply ~p)", [Mail, Pass, Any]),
            none
    end.


add_user(Pool, Mail, FName, LName, Pass) ->
    persist_pgsql:qe(Pool, "INSERT INTO users(mail, fname, lname, pass) VALUES ($1, $2, $3, $4);", 
        [Mail, FName, LName, Pass]).


update_user(Pool, Mail, Pass) ->
    persist_pgsql:qe(Pool, "UPDATE users SET pass=$1 WHERE mail=$2;", [Pass, Mail]).


del_user(Pool, Mail) ->
    persist_pgsql:qe(Pool, "UPDATE users SET en=FALSE WHERE mail=$1;", [Mail]).

%
% social's op
%


