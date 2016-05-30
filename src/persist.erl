-module(persist).
-export([
          init_db/1, 
          init_db/2,
          check_user/2, 
          get_user/2, 
          get_user/3, 
          add_user/5, 
          update_user/3, 
          del_user/2,
          has_social_link/2, 
          add_social_link/3, 
          update_social_link/3, 
          del_social_link/2, 
          list_social_link/2,
          get_payment/3,
          add_payment/4,
          set_payment_state/6
        ]).

-include_lib("wa.hrl").

-define(OR_EMPTY(Action), case Action of {ok, {_, Strs}} -> Strs; Any -> ?ERROR("~p", [Any]), [] end).

%
% init db ( persist:init_db(pgdb). )
%

init_db(Pool) ->
  [init_db(I, Pool) || I <- [1, 2, 3]].

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
          "DROP TABLE users_social;",
          "CREATE TABLE users_social (
                  id varchar(256),
                  mail varchar(128),
                  title varchar(256),
                  token varchar(512),
                  en boolean NOT NULL DEFAULT TRUE,
                  atime timestamp DEFAULT current_timestamp
              );",
          "CREATE INDEX users_social_id_idx ON users_social(id);",
          "CREATE INDEX users_social_mail_idx ON users_social(mail, en);",
          "CREATE INDEX users_social_id_en_idx ON users_social(id, en);",
          "CREATE INDEX users_social_id_mail_en_idx ON users_social(id, mail, en);"
       ]);
init_db(3, Pool) ->
  persist_pgsql:ql(Pool, [
          %
          % Payments
          % 
          "DROP TYPE payment_state CASCADE;",
          "DROP TABLE payments;",
          "CREATE TYPE payment_state AS ENUM('start', 'fail', 'success');",
          "CREATE TABLE payments (
              id serial PRIMARY KEY,
              mail varchar(128),
              state payment_state DEFAULT 'start',
              token varchar(128),
              token_reply jsonb,
              details_reply jsonb DEFAULT '{}',
              result_reply jsonb DEFAULT '{}',
              atime timestamp DEFAULT current_timestamp
          );",
          "CREATE INDEX payments_state_idx ON payments(state);",
          "CREATE INDEX payments_mail_idx ON payments(mail);",
          "CREATE INDEX payments_token_idx ON payments(token);"
      ]).

%
% user's ops
%

check_user(Pool, Mail) ->
  case persist_pgsql:qe(Pool, 
      "SELECT COUNT(mail) AS c FROM users WHERE en=TRUE AND mail=$1", 
      [Mail]) of
    {ok, {_, [{Count}]}} when Count =:= 1 -> true;
    _Any -> false
  end.

get_user(Pool, Mail, Pass) ->
  case persist_pgsql:qe(Pool, 
      "SELECT mail, fname, lname FROM users WHERE en=TRUE AND mail=$1 AND pass=$2", 
      [Mail, Pass]) of
    {ok, {_, [{Mail, FName, LName}]}} -> {Mail, FName, LName};
    _Any -> none
  end.

get_user(Pool, Mail) ->
  case persist_pgsql:qe(Pool, 
      "SELECT mail, fname, lname FROM users WHERE en=TRUE AND mail=$1", 
      [Mail]) of
    {ok, {_, [{Mail, FName, LName}]}} -> {Mail, FName, LName};
    _Any -> none
  end.

add_user(Pool, Mail, FName, LName, Pass) ->
  persist_pgsql:qe(Pool, 
    "INSERT INTO users(mail, fname, lname, pass) VALUES ($1, $2, $3, $4);", 
    [Mail, FName, LName, Pass]).

update_user(Pool, Mail, Pass) ->
  persist_pgsql:qe(Pool, 
    "UPDATE users SET pass=$1 WHERE mail=$2;", 
    [Pass, Mail]).

del_user(Pool, Mail) ->
  persist_pgsql:qe(Pool, 
    "UPDATE users SET en=FALSE WHERE mail=$1;", 
    [Mail]).

%
% social's op
%

has_social_link(Pool, Id) ->
  case persist_pgsql:qe(Pool, 
      "SELECT mail FROM users_social WHERE en=TRUE AND id=$1", 
      [Id]) of
    {ok, {_, [{Mail}]}} -> Mail;
    _Any -> none
  end.

add_social_link(Pool, Mail, Opts) ->
  persist_pgsql:qe(Pool, 
    "INSERT INTO users_social(id, mail, title, token) VALUES ($1, $2, $3, $4);", 
    [maps:get(id, Opts), Mail, maps:get(title, Opts), maps:get(token, Opts)]).

update_social_link(Pool, Mail, Opts) ->
  persist_pgsql:qe(Pool, 
    "UPDATE users_social SET mail=$1, title=$2, token=$3 WHERE id=$4 AND en=TRUE;", 
    [Mail, maps:get(title, Opts), maps:get(token, Opts), maps:get(id, Opts)]).

del_social_link(Pool, Id) ->
  persist_pgsql:qe(Pool, 
    "UPDATE users_social SET en=FALSE WHERE id=$1;", 
    [Id]).

list_social_link(Pool, Mail) ->
  case persist_pgsql:qe(Pool, 
      "SELECT id, title, token FROM users_social WHERE en=TRUE AND mail=$1", 
      [Mail]) of
    {ok, {H, List}} -> persists:to_map(H, List);
    _Any -> []
  end.

%
% payments
%

get_payment(Pool, Mail, Token) ->
  {ok, {Cols, Rows}} = persist_pgsql:qe(Pool, 
    "SELECT * FROM payments WHERE token=$1 AND mail=$2;", 
    [Token, Mail]),
  M = iomod:take_one(persist_pgsql:to_map(Cols, Rows)),
  iomod:cascade(M, [<<"token_reply">>, <<"details_reply">>, <<"result_reply">>], in). 

add_payment(Pool, Mail, Token, TokenReply) ->
  persist_pgsql:qe(Pool, 
    "INSERT INTO payments(mail, token, token_reply) VALUES ($1, $2, $3);", 
    [Mail, Token, iomod:out(TokenReply)]).

set_payment_state(Pool, Mail, Token, State, DetailReply, ResultReply) ->
  persist_pgsql:qe(Pool, 
    "UPDATE payments SET state=$1, details_reply=$2, result_reply=$3 WHERE token=$4 AND mail=$5;", 
    [State, iomod:out(DetailReply), iomod:out(ResultReply), Token, Mail]).

