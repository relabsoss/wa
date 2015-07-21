Goals
=====

This project was targetting on creation the starting point for web-developers for using Erlang + PostgreSQL + Redis as platform.

Installation quest
------------------

You need install PostgreSQL (9.4 or higher), Erlang (better R17, R18 has some unpleasand issues with deps). Redis currently not used, but we can. :)

Next - install and build application:

    git clone https://github.com/mkrentovskiy/wa.git
    cd wa
    make
    ...
    (here we can get troubles with merl cause some rebar species. Just cd deps/merl; make; nano ebin/merl.app 
    and add merl_tests.beam to modules, after that repeat make in application top)
    ...
    cp priv/sys.config priv/local.sys.config
    make run-local

... and its not work! :) We need some configuration steps:

Configure database:

    sudo su postgres -c psql
    postgres=# CREATE DATABASE wa ENCODING 'utf8';
    postgres=# CREATE USER wa WITH PASSWORD 'wa';
    postgres=# GRANT ALL PRIVILEGES ON DATABASE wa TO wa;
    postgres=# \q

Next, Mailgun. Go to https://mailgun.com/, register, add some domain, confirm it and get API access params - 
URL, public and private keys. Write it to local.sys.config at mail_sender section. (Some lyrics: most of public services 
are block templated messages as a spam. If you need SMTP sender, you can take this one - https://github.com/mkrentovskiy/onelogin/blob/master/src/mail_sender.erl)

ReCAPTCHA 2 is using for auto-registration block. You need open and secret key for it. Go to https://www.google.com/recaptcha/admin#list, add your 
application, put localhost to allowed domains. Than replace open key in https://github.com/mkrentovskiy/wa/blob/master/priv/www/index.html#L92 and add 
secret key to local.sys.config into recaptcha_key param.

Next step - http://dadata.ru/ (data quality as a service). If you don't need its suggestions, just remove it code from 
https://github.com/mkrentovskiy/wa/blob/master/priv/www/index.html#L143-L147 and https://github.com/mkrentovskiy/wa/blob/master/priv/www/js/app.js#L30-L40. 
Otherwise you must get your own personal API key from it site. Don't be a jerk using my one. :) Key must be writen to 
https://github.com/mkrentovskiy/wa/blob/master/priv/www/index.html#L147.

Now main external API is done. Lets go for OAuth providers.
* Facebook: register application for web on https://developers.facebook.com/apps and add localhost to approoved domains
* Google: go to https://console.developers.google.com/project, add new project, then go to "APIs & auth -> Credetials" and create new client ID. 
Set up Redirect URIs to http://localhost:8080/oauth/google/callback, take client ID and secret to local.sys.config.
* VKontakte: https://vk.com/apps?act=manage, create application as web-site and set up URL to http://localhost:8080. Take id and key for config.

Almost done? Yep.

    make run-local
    1> persist:init_db(pgdb).

Now you can try to open http://localhost:8080 in your browser. Must work.
